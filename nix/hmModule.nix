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
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      cfg.package
      emacs-all-the-icons-fonts
      iosevka-bin
      (iosevka-bin.override { variant = "aile"; })
    ];
    xdg.configFile."emacs/early-init.el".source = "${cfg.package.emacs_d}/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      ${cfg.extraConfig}
      (load "default")
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
