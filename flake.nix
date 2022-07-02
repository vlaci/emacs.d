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
    ligature.url = "github:mickeynp/ligature.el";
    ligature.flake = false;
    evil-markdown.url = "github:Somelauw/evil-markdown";
    evil-markdown.flake = false;
    org.url = "github:bzg/org-mode";
    org.flake = false;
    org-modern-indent.url = "github:jdtsmith/org-modern-indent";
    org-modern-indent.flake = false;
    org-roam-ui.url = "github:org-roam/org-roam-ui";
    org-roam-ui.flake = false;
    mu4e-thread-folding.url = "github:rougier/mu4e-thread-folding";
    mu4e-thread-folding.flake = false;
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
        packages = pkgs;
        defaultPackage = pkgs.emacsVlaci;
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
