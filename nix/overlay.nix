{ inputs }:

final: prev:
let
  eo = inputs.emacs-overlay.overlay final prev;
  customEmacsPackages = final.callPackage ./customEmacsPackages.nix { inherit inputs; };
  emacs = (final.emacsPgtk.override {
    treeSitterPlugins = with final.tree-sitter-grammars; [
      tree-sitter-bash
      tree-sitter-beancount
      tree-sitter-c
      tree-sitter-c-sharp
      tree-sitter-cmake
      tree-sitter-cpp
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-elisp
      tree-sitter-go
      tree-sitter-gomod
      tree-sitter-graphql
      tree-sitter-haskell
      tree-sitter-html
      tree-sitter-http
      tree-sitter-java
      tree-sitter-javascript
      tree-sitter-json
      tree-sitter-julia
      tree-sitter-lua
      tree-sitter-make
      tree-sitter-nix
      tree-sitter-ocaml
      tree-sitter-python
      tree-sitter-regex
      tree-sitter-ruby
      tree-sitter-rust
      tree-sitter-scala
      tree-sitter-toml
      tree-sitter-tsx
      tree-sitter-typescript
      tree-sitter-yaml
      tree-sitter-zig
    ];
  }).overrideAttrs (_: {
    src = final.fetchFromSavannah {
      repo = "emacs";
      rev = "a8c9283e1702af06fb6ad598ae32a2c124860af1";
      hash = "sha256-tImHRMQXtlWbfgienaSmxMzKG3J8+LFeoOyMTLEcOSs=";
    };
  });
  fromElisp = (import "${inputs.fromElisp}") { pkgs = final; };
in
eo // fromElisp // {
  emacsPackagesFor = emacs: (eo.emacsPackagesFor emacs).overrideScope' customEmacsPackages;
  emacsVlaci = final.callPackage ./emacs.nix { inherit emacs inputs; };
  nuspell = prev.nuspell.overrideAttrs (super: { propagatedBuildInputs = super.buildInputs; });
}
