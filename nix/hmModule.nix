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
    package = mkOption { default = emacs.override { inherit (cfg) extraPackages; }; };
    extraConfig = mkOption { default = ""; };
    extraPackages = mkOption { default = _: []; };
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
      etBook

      direnv
      lorri

      nodePackages.bash-language-server
      nodePackages.typescript # https://github.com/emacs-lsp/lsp-mode/pull/2633
      nodePackages.vscode-css-languageserver-bin
      nodePackages.vscode-html-languageserver-bin
      nodePackages.vscode-json-languageserver
      pyright
      rnix-lsp
      rust-analyzer
    ];
    emacsVlaci.settings."mu4e-maildir" = config.accounts.email.maildirBasePath;
    emacsVlaci.extraPackages = with pkgs; (_: [ mu ]);

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
