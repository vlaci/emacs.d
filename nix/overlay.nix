{ inputs }:

final: prev:
let
  eo = inputs.emacs-overlay.overlay final prev;
  customEmacsPackages = final.callPackage ./customEmacsPackages.nix { inherit inputs; };
  emacs = final.emacsPgtkNativeComp;
  parse = final.callPackage "${inputs.emacs-overlay}/parse.nix" { };
in
eo // {
  emacsPackagesFor = emacs: (eo.emacsPackagesFor emacs).overrideScope' customEmacsPackages;
  emacsVlaci = final.callPackage ./emacs.nix { inherit emacs inputs parse; };
}
