{ lib
, pkgs
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
, proselint
, fd
, jre
, languagetool
, mu
, msmtp
, haskell
, nodejs-slim
, nodePackages
, ocamlformat
, ocamlPackages
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

  parse = pkgs.callPackage "${inputs.emacs-overlay}/parse.nix" { };
  dicts = with hunspellDicts; [ hu-hu en-us-large ];
  emacs-nix-integration =
    let
      binaries = [
        fd
        proselint
        jre
        languagetool
        mu
        msmtp
        # lsp
        haskell.packages.ghc942.haskell-language-server
        ocamlPackages.ocaml-lsp
        pyright
        rnix-lsp
        rust-analyzer
        sumneko-lua-language-server
        zls
        # formatters for apheleia
        black
        clang-tools
        isort
        nixpkgs-fmt
        nodePackages.prettier
        ocamlformat
      ];
      makeElispExecPath = paths: with lib; "(list" + (concatStringsSep " " (map (path: "\"${path}/bin\"") (filter (x: x != null) paths))) + ")";
      extra_exec_path = makeElispExecPath binaries;
    in
    emacsPackages.trivialBuild {
      pname = "nix-integration";
      src = substituteAll {
        src = ./nix-integration.el;
        inherit extra_exec_path;
      };
    };

  src = impure.init_d or builtins.path { name = "init.d"; path = ../.; };

  packages =
    let
      files = builtins.filter (f: lib.hasSuffix ".el" f) (lib.filesystem.listFilesRecursive src);
    in
    lib.flatten (map
      (f:
        let
          configText = builtins.readFile f;
        in
          parse.parsePackagesFromUsePackage { inherit configText; alwaysEnsure = true; })
      files);

  emacsPackages = emacsPackagesFor emacs;
  emacsWithPackages = emacsPackages.emacsWithPackages;

  mkEmacs = extraPkgs:
    emacsWithPackages (epkgs:
      (map (n: epkgs.${n}) packages)
      ++ (extraPkgs epkgs) ++ [ emacs-nix-integration ]
    );

  emacsStage1 = mkEmacs (epkgs: [ ]);

  init_d = impure.init_d or (runCommand "emacs.d"
    {
      src = builtins.path { name = "init.d"; path = ../.; filter = path: type: type == "directory" || builtins.any (suffix: lib.hasSuffix suffix path) [ ".el" ".eld" ".png" ]; };
      buildInputs = [ emacsStage2 ];
    } ''
    cp -r $src/* .
    chmod -R u+w .

    echo "-- Generating autoloads for package quickstart..."
    emacs \
      --batch \
      --quick \
      -l package \
      --eval '(let ((package-quickstart-file "elisp/autoloads.el"))
                (defun byte-compile-file (f))
                (package-quickstart-refresh))'

    echo "-- Generating autoloads for local packages..."
    emacs -L elisp -L modules --batch -f loaddefs-generate-batch elisp/local-autoloads.el elisp

    echo "-- Byte compiling elisp files..."
    emacs -L elisp -L modules --batch -f batch-byte-compile {,modules,elisp/}*.el

    mkdir -p $out
    cp -r * $out
    export EMACSNATIVELOADPATH=$out/eln-cache
    echo "-- Native compiling elisp files..."
    emacs -L $out/elisp --batch -f batch-native-compile $out/elisp/*.el
  '');

  emacsStage2 = mkEmacs (epkgs:
    let
      packagesFun = epkgs:
        map (n: epkgs.${n}) packages;
      extra = extraPackages epkgs;
    in
    extra
  );

  dictSearchPath = lib.makeSearchPath "share/hunspell" dicts;
in
emacsStage2.overrideAttrs (super: {
  buildCommand = super.buildCommand + ''
    wrapProgram $out/bin/emacs \
                --append-flags "--init-directory ${toString init_d}" \
                --prefix DICPATH : ${lib.escapeShellArg dictSearchPath}
  '';
  passthru = { inherit init_d emacs-nix-integration; };
})
