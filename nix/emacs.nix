{ lib
, inputs
, autoPatchelfHook
, emacsPackagesFor
, emacs
, fetchFromGitHub
, runCommand
, stdenv
, libkrb5
, zlib
, lttng-ust
, writeText
, graphviz
, vscode-extensions
, clang-tools
, hunspellDicts
, hunspellWithDicts
, jre
, languagetool
, mu
, msmtp
, nodejs-slim
, nodePackages
, sumneko-lua-language-server
, pyright
, rnix-lsp
, rust-analyzer
, zls
, parse
, extraPackages ? (_: [])
}:

let
  emacs-nixos-integration =
    let
      hunspell = hunspellWithDicts (with hunspellDicts; [ hu-hu en-us ]);
    in
    writeText "nixos-integration.el" ''
      (setq-default ispell-program-name "${hunspell}/bin/hunspell")
      (setq-default langtool-java-bin "${jre}/bin/java"
                    langtool-language-tool-jar "${languagetool}/share/languagetool-commandline.jar"
                    mu4e-binary "${mu}/bin/mu"
                    sendmail-program "${msmtp}/bin/msmtp")
      (setq-default vl--eglot-pyright-executable "${pyright}/bin/pyright-langserver"
                    vl--eglot-rust-analyzer-executable "${rust-analyzer}/bin/rust-analyzer")
    '';

  runtimeDependencies = [
    clang-tools
    nodePackages.typescript-language-server
    sumneko-lua-language-server
    nodePackages.unified-language-server
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    nodePackages.vscode-json-languageserver
    rnix-lsp
    zls
  ];

  packages = parse.parsePackagesFromUsePackage {
    configText = builtins.readFile ../README.org;
    isOrgModeFile = true;
    alwaysTangle = true;
    alwaysEnsure = true;
  };

  emacsPackages = emacsPackagesFor emacs;
  emacsWithPackages = emacsPackages.emacsWithPackages;

  mkEmacs = extraPkgs:
    emacsWithPackages (epkgs:
      (map (n: epkgs.${n}) packages)
      ++ (extraPkgs epkgs)
    );

  emacsStage1 = mkEmacs (epkgs: [ epkgs.use-package ]);


  emacs_d = runCommand "mk-emacs.d"
    {
      buildInputs = [ emacsStage1 ];
      literateInitFile = ../README.org;
    } ''
    mkdir -p $out
    cp $literateInitFile $out/init.org
    # using unconfigured emacs so that nothing can hook into tangling (looking at you, orgit)
    ${emacs}/bin/emacs --batch --quick -l ob-tangle --eval '(progn (defvar org-element-cache-persistent nil)(org-babel-tangle-file "'$out'/init.org"))'
    rm $out/init.org
    emacs --batch --quick -l package --eval '(let ((package-quickstart-file "'$out'/autoloads.el")) (defun byte-compile-file (f)) (package-quickstart-refresh))'
  '';

  emacsStage2 = mkEmacs (epkgs: let
    default = epkgs.trivialBuild {
      pname = "default";
      packageRequires = emacsStage1.deps.explicitRequires;
      unpackPhase = ''
        cp ${emacs-nixos-integration} ./nixos-integration.el
        cp ${emacs_d}/{*.el,*.elc} .
        cat > default.el <<EOF
        (load "nixos-integration")
        (setq package-quickstart-file "$out/share/emacs/site-lisp/autoloads.el")
        (load "init")
        EOF
      '';
    };
    extra = extraPackages epkgs;
  in
    [ default ] ++ extra
  );
in
emacsStage2.overrideAttrs (super: {
  buildCommand = super.buildCommand + ''
    wrapEmacs() {
        wrapProgram "$1" --suffix PATH : "${lib.makeBinPath runtimeDependencies}"
    }

    for prog in $out/bin/*; do
        wrapEmacs $prog
    done
  '';
  passthru = { inherit emacs_d emacs-nixos-integration; };
})
