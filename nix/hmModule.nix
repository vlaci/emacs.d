{ overlay }:
{ config, lib, pkgs, ... }:

let
  inherit (lib) concatMapStringsSep generators isAttrs isBool isInt isList mkIf mkOption mkEnableOption types;
  emacs = (pkgs.appendOverlays [ overlay ]).emacsVlaci;
  cfg = config.emacsVlaci;

  toEmacsLisp = value:
    if isBool value then
      (if value then "t" else "nil")
    else if isInt value then
      toString value
    else if isList value then
      let
        car = builtins.head value;
        tail = concatMapStringsSep " " toEmacsLisp (builtins.tail value);
        symbols = {
          list = "(list ${tail})";
          quote = "'(${tail})";
          backquote = "`(${tail}})";
          cons = "(${toEmacsLisp (builtins.elemAt value 1)} . ${toEmacsLisp (builtins.elemAt value 2)})";
        };
      in
        symbols.${car} or "(${car}\n${tail})"
    else if isAttrs value && value ? elisp then
      value.elisp
    else if isAttrs value then
      generators.toKeyValue
        {
          mkKeyValue = key: value: "  ${key} ${toEmacsLisp value}";
        }
        value
    else
      ''"${toString value}"'';
  toEmacsConfig = settings: toEmacsLisp [ "+set-defaults!" settings ];
in
{
  options.emacsVlaci = {
    enable = mkEnableOption { };
    package = mkOption { default = emacs.override { inherit (cfg) extraPackages; }; };
    extraConfig = mkOption { default = ""; };
    extraPackages = mkOption { default = _: [ ]; };
    settings = mkOption {
      type = with types; attrsOf anything;
      default = { };
    };
  };
  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      emacs-all-the-icons-fonts
      iosevka-comfy.comfy
      iosevka-comfy.comfy-duo
      bitwarden-cli

      direnv
    ];
    emacsVlaci.settings."mu4e-maildir" = config.accounts.email.maildirBasePath;
    emacsVlaci.extraPackages = with pkgs; (_: [ mu ]);

    programs.emacs = {
      enable = true;
      inherit (cfg) package;
    };

    services.emacs = {
      enable = true;
      client.enable = true;
      socketActivation.enable = true;
    };

    xdg.configFile."emacs/etc/nix-settings.el".text = ''
      ${cfg.extraConfig}
      ${toEmacsConfig cfg.settings}
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
