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
, fromElisp
, extraPackages ? (_: [])
, impure ? {}
}:

let
  emacs-nix-integration =
    let
      hunspell = hunspellWithDicts (with hunspellDicts; [ hu-hu en-us ]);
    in
      substituteAll {
        src = ./nix-integration.el;
        inherit hunspell jre languagetool mu msmtp pyright;
        rustanalyzer = rust-analyzer;
      };

  src = impure.init_d or ../.;

  parsePackages = configText: let
    name = item: let
      pkg = builtins.head item;
      isQuoted = (builtins.head pkg) == "quote";
    in
      if (builtins.length item == 1) && isQuoted then
        [ (builtins.elemAt pkg 1) ]
      else
        []
      ;
    recurse = item:
      if builtins.isList item && item != [] then
        let
          head = builtins.head item;
          tail = builtins.tail item;
        in
          if head == "package-install" then
            name tail
          else
            map recurse item
      else
        [];
  in
    lib.flatten  (map recurse (fromElisp configText));

  packages = parsePackages (builtins.readFile (src + "/elisp/packages.el"));

  emacsPackages = emacsPackagesFor emacs;
  emacsWithPackages = emacsPackages.emacsWithPackages;

  mkEmacs = extraPkgs:
    emacsWithPackages (epkgs:
      (map (n: epkgs.${n}) (lib.traceValSeq packages))
      ++ (extraPkgs epkgs)
    );

  emacsStage1 = mkEmacs (epkgs: [ ]);

  init_d = impure.init_d or (runCommand "emacs.d" {
    src = ../.;
    buildInputs = [ emacsStage2 ];
  } ''
    cp -r $src/{elisp,*.el} .
    chmod -R u+w .
    find
    emacs \
      --batch \
      --quick \
      -l package \
      --eval '(let ((package-quickstart-file "elisp/autoloads.el"))
                (defun byte-compile-file (f))
                (package-quickstart-refresh))'
    emacs -L . -L elisp --batch -f batch-byte-compile {,elisp/}*.el

    mkdir -p $out
    cp -r * $out

  '');

  emacsStage2 = mkEmacs (epkgs: let
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
