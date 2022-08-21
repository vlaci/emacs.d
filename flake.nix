{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    flake-utils.url = "github:numtide/flake-utils";
    fromElisp.url = "github:talyz/fromElisp";
    fromElisp.flake = false;
    ligature.url = "github:mickeynp/ligature.el";
    ligature.flake = false;
    eglot-x.url = "github:nemethf/eglot-x";
    eglot-x.flake = false;
    explain-pause-mode.url = "github:lastquestion/explain-pause-mode";
    explain-pause-mode.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    rec {
      overlay = import ./nix/overlay.nix { inherit inputs; };
      lib.hmModule = import ./nix/hmModule.nix { inherit overlay; };
    } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in
      {
        legacyPackages = pkgs;
        packages = { inherit (pkgs) emacsVlaci; };
        defaultPackage = pkgs.emacsVlaci;
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
