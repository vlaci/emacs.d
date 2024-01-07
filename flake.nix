{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    fromElisp.url = "github:talyz/fromElisp";
    fromElisp.flake = false;

    nix-filter.url = "github:numtide/nix-filter";

    flake-parts.url = "github:hercules-ci/flake-parts";

    on.url = "github:ajgrf/on.el";
    on.flake = false;
    ws-butler.url = "github:hlissner/ws-butler";
    ws-butler.flake = false;
    emacs-lsp-booster.url = "github:blahgeek/emacs-lsp-booster";
    emacs-lsp-booster.flake = false;
    explain-pause-mode.url = "github:lastquestion/explain-pause-mode";
    explain-pause-mode.flake = false;
  };
  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      let
        flakeModules.default = import ./flake-module.nix;
      in
      {
        debug = true;
        systems = [ "x86_64-linux" ];
        imports = [
          flake-parts.flakeModules.easyOverlay
          flakeModules.default
        ];
        perSystem = { inputs', config, pkgs, system, ... }: {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.emacs-overlay.overlays.default
            ];
          };
          vl-emacs = {
            package = inputs'.emacs-overlay.packages.emacs-pgtk;
            withAllTreesitGrammars = true;
            initDirectory = ./emacs.d;
            extraPackages = [ "setup" "vl-setup" ];
            localPackages = epkgs: {
              vl-setup = { src = ./elisp/vl-setup.el; };
              vl-modeline = { src = ./elisp/vl-modeline.el; };
              on = { src = inputs.on; };
              mini-echo = { src = inputs.mini-echo; };
              awesome-tray = { src = inputs.awesome-tray; };
              lsp-bridge = {
                inherit (pkgs.emacsPackages.lsp-bridge) patches recipe;
                src = inputs.lsp-bridge;
                packageRequires = with epkgs; [ acm markdown-mode ];
              };
              acm = {
                src = inputs.lsp-bridge;
                inherit (pkgs.emacsPackages.acm) recipe;
                packageRequires = [ epkgs.yasnippet ];
              };
              explain-pause-mode = { src = inputs.explain-pause-mode; };
            };
            overrides = final: prev: {
              lsp-mode = prev.lsp-mode.overrideAttrs (_: {
                postPatch = ''
                  substituteInPlace lsp-protocol.el \
                    --replace '(getenv "LSP_USE_PLISTS")' 't'
                '';
              });
              eglot = null;
              jinx = prev.jinx.overrideAttrs (_: {
                buildInputs = [ pkgs.enchant ];
                preBuild = ''
                  cc -I -O2 -Wall -Wextra -fPIC -shared -I${pkgs.enchant.dev}/include/enchant-2 -lenchant-2 -o $NIX_BUILD_TOP/jinx-mod.so jinx-mod.c
                '';
                postInstall = ''
                  install -m 444 jinx-mod.so $out/share/emacs/site-lisp/elpa/jinx-*
                '';
              });
              ws-butler = prev.ws-butler.overrideAttrs (_: {
                src = inputs.ws-butler;
              });
            };
          };
          packages.default = config.vl-emacs;
          apps.default = { type = "app"; program = "${config.vl-emacs}/bin/emacs"; };
        };
      }
    );
}
