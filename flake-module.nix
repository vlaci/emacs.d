{ lib, flake-parts-lib, config, inputs, ... }:

let
  inherit (lib) mkIf;
  inherit (flake-parts-lib) importApply mkPerSystemOption;
in
{
  options = {
    perSystem = mkPerSystemOption (importApply ./flake-module-per-system.nix { inherit inputs; });
  };
  config = mkIf config.vl-emacs.enable { };
}
