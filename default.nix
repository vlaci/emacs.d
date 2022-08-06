let
  flake = builtins.getFlake (toString ./.);
in
{ pkgs ? flake.legacyPackages.${builtins.currentSystem} }:

pkgs.emacsVlaci.override { impure.init_d = ./.; }
