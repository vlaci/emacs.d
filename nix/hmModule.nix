{ overlay }:
{ config, lib, pkgs, ...}:

let
  inherit (lib) concatMapStringsSep generators isAttrs isBool isInt isList mkIf mkOption mkEnableOption types;
  emacs = (pkgs.appendOverlays [ overlay ]).emacsVlaci;
  cfg = config.emacsVlaci;

  toEmacsLisp = value:
        if isBool value then
          (if value then "t" else "nil")
        else if isInt value then
          value
        else if isList value then
          '''(${concatMapStringsSep " " toEmacsLisp value})''
        else if isAttrs value && value ? elisp then
          value.elisp
        else
          ''"${toString value}"'';
  toEmacsConfig = generators.toKeyValue {
    mkKeyValue = key: value:
      let
        value' = toEmacsLisp value;
      in "(setq ${key} ${value'})";
  };
in {
  options.emacsVlaci = {
    enable = mkEnableOption {};
    package = mkOption { default = emacs; };
    extraConfig = mkOption { default = ""; };
    settings = mkOption {
      type = with types; attrsOf anything;
      default = { };
    };
  };
  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      cfg.package
      emacs-all-the-icons-fonts
      iosevka-bin
      (iosevka-bin.override { variant = "aile"; })

      direnv
      lorri

      nodePackages.bash-language-server
      pyright
      rnix-lsp
      rust-analyzer
    ];
    xdg.configFile."emacs/early-init.el".source = "${cfg.package.emacs_d}/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      ${cfg.extraConfig}
      ${toEmacsConfig cfg.settings}
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
