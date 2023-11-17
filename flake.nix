{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    fromElisp.url = "github:talyz/fromElisp";
    fromElisp.flake = false;

    nix-filter.url = "github:numtide/nix-filter";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      let
        flakeModules.default = import ./flake-module.nix;
      in
      {
        debug = true;
        systems = [ "x86_64-linux" ];
        imports = [
          flake-parts.flakeModules.easyOverlay
          flakeModules.default
        ];
        perSystem = { inputs', config, ... }: {
          vl-emacs = {
            package = inputs'.emacs-overlay.packages.emacs-pgtk;
            withAllTreesitGrammars = false;
            initDirectory = ./emacs.d;
            extraPackages = [ "setup" ];
            localPackages = {
              vlaci = { src = ./elisp/vlaci.el; };
            };
          };
          packages.default = config.vl-emacs;

        };
      }
    );
}
