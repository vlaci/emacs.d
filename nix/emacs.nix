{ lib
, inputs
, autoPatchelfHook
, emacsPackagesFor
, emacs
, fetchFromGitHub
, runCommand
, substituteAll
, stdenv
, libkrb5
, zlib
, lttng-ust
, writeText
, graphviz
, vscode-extensions
, hunspellDicts
, hunspellWithDicts
, proselint
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
, black
, clang-tools
, isort
, nixpkgs-fmt
, fromElisp
, extraPackages ? (_: [ ])
, impure ? { }
}:

let
  emacs-nix-integration =
    let
      hunspell = hunspellWithDicts (with hunspellDicts; [ hu-hu en-us ]);
      binaries = [
        hunspell
        proselint
        jre
        languagetool
        mu
        msmtp
        # lsp
        pyright
        rnix-lsp
        rust-analyzer
        sumneko-lua-language-server
        zls
        # formatters for prettier
        black
        clang-tools
        isort
        nixpkgs-fmt
        nodePackages.prettier
      ];
      makeElispExecPath = paths: with lib; "(list" + (concatStringsSep " " (map (path: "\"${path}/bin\"") (filter (x: x != null) paths))) + ")";
      extra_exec_path = makeElispExecPath binaries;
    in
    substituteAll {
      src = ./nix-integration.el;
      inherit extra_exec_path;
    };

  src = impure.init_d or ../.;

  parsePackages = configText:
    let
      name = builtins.head;
      recurse = item:
        if builtins.isList item && item != [ ] then
          let
            head = builtins.head item;
            tail = builtins.tail item;
          in
          if head == "+install!" then
            name tail
          else
            map recurse item
        else
          [ ];
    in
    lib.flatten (map recurse (fromElisp configText));

  packages =
    let
      files = builtins.filter (f: lib.hasSuffix ".el" f) (lib.filesystem.listFilesRecursive src);
    in
    lib.flatten (map
      (f:
        let
          text = builtins.readFile f;
        in
        parsePackages text)
      files);

  emacsPackages = emacsPackagesFor emacs;
  emacsWithPackages = emacsPackages.emacsWithPackages;

  mkEmacs = extraPkgs:
    emacsWithPackages (epkgs:
      (map (n: epkgs.${n}) packages)
      ++ (extraPkgs epkgs)
    );

  emacsStage1 = mkEmacs (epkgs: [ ]);

  init_d = impure.init_d or (runCommand "emacs.d"
    {
      src = ../.;
      buildInputs = [ emacsStage2 ];
    } ''
    cp -r $src/{elisp,templates,*.el} .
    chmod -R u+w .
    cp ${emacs-nix-integration} elisp/nix-integration.el
    find
    emacs \
      --batch \
      --quick \
      -l package \
      --eval '(let ((package-quickstart-file "elisp/autoloads.el"))
                (defun byte-compile-file (f))
                (package-quickstart-refresh))'

    mkdir -p $out
    export EMACSNATIVELOADPATH=$out/eln-cache
    emacs -L elisp --batch -f batch-byte-compile {,elisp/}*.el
    cp -r * $out
    #emacs -L $out/elisp --batch -f batch-native-compile $out/{,elisp/}*.el
  '');

  emacsStage2 = mkEmacs (epkgs:
    let
      packagesFun = epkgs:
        map (n: epkgs.${n}) packages;
      extra = extraPackages epkgs;
    in
    extra
  );
in
emacsStage2.overrideAttrs (super: {
  buildCommand = super.buildCommand + ''
    wrapProgram $out/bin/emacs --append-flags "--init-directory ${toString init_d}"
  '';
  passthru = { inherit init_d emacs-nix-integration; };
})
