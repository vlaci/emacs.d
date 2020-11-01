{ stdenv
, emacsGcc
, emacsPackagesFor
, writeText
, nix-straight
}:

let
  straight-env = nix-straight rec {
    emacsPackages = emacsPackagesFor emacsGcc;
    emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [ use-package ]);
    emacsInitFile = ./init.el;
    emacsLoadFiles = [
      (writeText "quirks.el" ''
        (with-eval-after-load "straight"
          (straight-use-package-mode +1))
      '')
    ];
    emacsArgs = [ "--debug-init" ];
  };

  emacsEnv = straight-env.emacsEnv { };
in
emacsEnv
