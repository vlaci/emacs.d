{ inputs }:

final: prev:
let
  eo = inputs.emacs-overlay.overlay final prev;
  customEmacsPackages = final.callPackage ./customEmacsPackages.nix { inherit inputs; };
  emacs = final.emacs-pgtk;
  fromElisp = (import "${inputs.fromElisp}") { pkgs = final; };
in
eo // fromElisp // {
  emacsPackagesFor = emacs: (eo.emacsPackagesFor emacs).overrideScope' customEmacsPackages;
  emacsVlaci = final.callPackage ./emacs.nix { inherit emacs inputs; };
  nil = inputs.nil.packages.${final.system}.default;
}
