{ overlay }:
{ config, lib, pkgs, ...}:

let
  inherit (lib) mkIf mkOption mkEnableOption;
  emacs = (pkgs.appendOverlays [ overlay ]).emacsVlaci;
  cfg = config.emacsVlaci; 
in {
  options.emacsVlaci = {
    enable = mkEnableOption {};
    package = mkOption { default = emacs; };
    extraConfig = mkOption { default = ""; };
  };
  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."emacs/early-init.el".source = "${cfg.package.emacs_d}/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      (load "default")
      ${cfg.extraConfig}
    '';
    xdg.desktopEntries."org-protocol" = {
      name = "Org-Protocol";
      exec = "emacsclient %u";
      icon = "emacs-icon";
      type = "Application";
      mimeType = [ "x-scheme-handler/org-protocol" ];
    };
    xdg.mimeApps.defaultApplications = {
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
    };
  };
}
