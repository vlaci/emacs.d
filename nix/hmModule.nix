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

  commonShellInit = ''
    vterm_printf(){
      if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
      elif [ "''${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
      else
        printf "\e]%s\e\\" "$1"
      fi
    }

    vterm_prompt_end() {
      vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }

    if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
        function clear() {
          vterm_printf "51;Evterm-clear-scrollback";
          tput clear;
        }
    fi
  '';
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
      (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
      iosevka-comfy.comfy
      iosevka-comfy.comfy-duo
      meslo-lgs-nf
      bitwarden-cli

      direnv
    ];
    emacsVlaci.settings."mu4e-maildir" = config.accounts.email.maildirBasePath;
    emacsVlaci.extraPackages = with pkgs; (_: [ mu ]);

    programs.emacs = {
      enable = true;
      inherit (cfg) package;
    };

    programs.bash.initExtra = ''
      ${commonShellInit}

      if [[ -n "$EAT_SHELL_INTEGRATION_DIR" ]]; then
        source "$EAT_SHELL_INTEGRATION_DIR/bash"
      fi

      if [[ ''${INSIDE_EMACS:-} = 'vterm' ]]; then
        PS1=$PS1'\[$(vterm_prompt_end)\]'
      fi
    '';

    programs.zsh.initExtra = lib.mkAfter ''
      ${commonShellInit}

      [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
        source "$EAT_SHELL_INTEGRATION_DIR/zsh"

      prompt_vterm_prompt_end() {
        p10k segment -c "$([[ ''${INSIDE_EMACS:-} = vterm ]] && echo 1)" -t "$(vterm_prompt_end)"
      }

      prompt_eat_prompt_start() {
        p10k segment -c "''${EAT_SHELL_INTEGRATION_DIR:-}" -t "$(printf '\e]51;e;B\e\\')"
      }

      prompt_eat_prompt_end() {
        p10k segment -c "''${EAT_SHELL_INTEGRATION_DIR:-}" -t "$(printf '\e]51;e;C\e\\')"
      }

      if [[ -n ''${INSIDE_EMACS:-} ]]; then
        if [[ -n "$POWERLEVEL9K_LEFT_PROMPT_ELEMENTS" ]]; then
          POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
            eat_prompt_start
            "''${POWERLEVEL9K_LEFT_PROMPT_ELEMENTS[@]}"
            eat_prompt_end vterm_prompt_end
          )
        fi
        if [[ ''${POWERLEVEL9K_TRANSIENT_PROMPT:-off} != 'off' ]]; then
            POWERLEVEL9K_TRANSIENT_PROMPT=off
            p10k reload
        fi
      fi
    '';

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
