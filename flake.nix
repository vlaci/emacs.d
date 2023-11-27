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
            package = inputs'.emacs-overlay.packages.emacs-pgtk.overrideAttrs (super: {
              patches = (super.patches or [ ]) ++ [
                ./emacs-async-json-rpc/0001-POC-Initial-implementation-of-async-json-rpc.patch
                ./emacs-async-json-rpc/0002-Add-MacOS-as-well.patch
                ./emacs-async-json-rpc/0003-Call-close-when-the-connection-is-done.patch
                ./emacs-async-json-rpc/0004-Add-some-synchronization-when-sending-receiving-noti.patch
                ./emacs-async-json-rpc/0005-Set-JSON_ALLOW_NUL-in-json_rpc_callback.patch
                ./emacs-async-json-rpc/0006-Initialize-state-done-within-json-rpc-connection.patch
                ./emacs-async-json-rpc/0007-Initialize-state-error_buffer_read.patch
                ./emacs-async-json-rpc/0008-Avoid-potential-deadlock.patch
                ./emacs-async-json-rpc/0009-Pass-environment-to-language-server.patch
                ./emacs-async-json-rpc/0010-Use-timeouts-on-all-json-rpc-handle-locks.patch
                ./emacs-async-json-rpc/0011-json_rpc_send_callback-invert-lock-order.patch
                ./emacs-async-json-rpc/0012-Elide-some-dynamic-memory-allocations.patch
                ./emacs-async-json-rpc/0013-Only-emit-a-single-space-after-Content-Length.patch
              ];
            });
            withAllTreesitGrammars = true;
            initDirectory = ./emacs.d;
            extraPackages = [ "setup" "vl-setup" ];
            localPackages = epkgs: {
              vl-setup = { src = ./elisp/vl-setup.el; };
              vl-modeline = { src = ./elisp/vl-modeline.el; };
              on = { src = inputs.on; };
              mini-echo = { src = inputs.mini-echo; };
              awesome-tray = { src = inputs.awesome-tray; };
            };
            overrides = final: prev: {
              lsp-mode = prev.lsp-mode.overrideAttrs (_: {
                postPatch = ''
                  substituteInPlace lsp-protocol.el \
                    --replace '(getenv "LSP_USE_PLISTS")' 't'
                '';
              });
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
