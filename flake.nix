{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    ligature.url = "github:mickeynp/ligature.el";
    ligature.flake = false;
    evil-markdown.url = "github:Somelauw/evil-markdown";
    evil-markdown.flake = false;
    vertico.url = "github:minad/vertico";
    vertico.flake = false;
    corfu.url = "github:minad/corfu";
    corfu.flake = false;
    embark.url = "github:oantolin/embark";
    embark.flake = false;
    org-roam.url = "github:org-roam/org-roam";
    org-roam.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    {
      overlay = import ./nix/overlay.nix { inherit inputs; };
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
