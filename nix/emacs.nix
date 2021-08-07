{ inputs
, emacsPackagesGen
, emacs
, runCommand
, writeText
, vscode-extensions
, clang-tools
, hunspellDicts
, hunspellWithDicts
, jre
, languagetool
, nodejs-slim
, nodePackages
, sumneko-lua-language-server
, parse
}:

let
  emacs-nixos-integration =
    let
      hunspell = hunspellWithDicts (with hunspellDicts; [ hu-hu en-us ]);
      cpptools = runCommand "cpptools"
        {
          cpptools = vscode-extensions.ms-vscode.cpptools;
        } ''
        mkdir -p $out
        ln -snf $cpptools/share/vscode/extensions/ms-vscode.cpptools $out/extension
      '';
    in
    writeText "nixos-integration.el" ''
      (setq-default ispell-program-name "${hunspell}/bin/hunspell")
      (setq-default langtool-java-bin "${jre}/bin/java"
                    langtool-language-tool-jar "${languagetool}/share/languagetool-commandline.jar")
      (setq-default dap-cpptools-debug-path "${cpptools}"
                    lsp-eslint-server-command (list "${nodejs-slim}/bin/node" "${vscode-extensions.dbaeumer.vscode-eslint}/share/vscode/extensions/dbaeumer.vscode-eslint/server/out/eslintServer.js" "--stdio")
                    lsp-clangd-binary-path "${clang-tools}/bin/clangd"
                    lsp-clients-typescript-tls-path "${nodePackages.typescript-language-server}/bin/typescript-language-server"
                    lsp-clients-lua-language-server-bin "${sumneko-lua-language-server}/bin/lua-language-server"
                    lsp-clients-lua-language-server-main-location "${sumneko-lua-language-server}/extras/main.lua")

       (advice-add 'lsp-css--server-command
                   :override (lambda () (list "${nodePackages.vscode-css-languageserver-bin}/bin/css-languageserver" "--stdio")))
    '';

  packages = parse.parsePackagesFromUsePackage {
    configText = builtins.readFile ../README.org;
    isOrgModeFile = true;
    alwaysTangle = true;
    alwaysEnsure = true;
  };

  emacsPackages = emacsPackagesGen emacs;
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
    emacs --batch --quick -l ob-tangle --eval '(org-babel-tangle-file "'$out'/init.org")'
    rm $out/init.org
    emacs --batch --quick -l package --eval '(let ((package-quickstart-file "'$out'/autoloads.el")) (package-quickstart-refresh))'
  '';

  emacsStage2 = mkEmacs (epkgs: let
        default = epkgs.trivialBuild {
          pname = "default";
          packageRequires = [
            epkgs.use-package
          ];
          buildPhase = ":";
          postInstall = ":";
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
      in
      [
        default
      ]);

  in
emacsStage2 // { inherit emacs_d emacs-nixos-integration; }
