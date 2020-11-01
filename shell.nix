{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    emacs-all-the-icons-fonts
    ((pkgs.emacsPackagesGen emacsGcc).emacsWithPackages (epkgs: with epkgs; [ vterm ]))
  ];
}
