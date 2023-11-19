{ inputs, ... }:

{ lib, flake-parts-lib, config, system, pkgs, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.vl-emacs;
  self =
    {
      _file = ./flake-module.nix;
      options = {
        vl-emacs = mkOption {
          type = with types; submodule ({ config, ... }: {
            options = {
              withAllTreesitGrammars = mkOption { type = types.bool; default = false; };
              package = mkOption { type = types.package; };
              initDirectory = mkOption { type = types.path; };
              extraPackages = mkOption { type = with types; listOf str; default = [ ]; };
              localPackages = mkOption { type = with types; attrsOf unspecified; default = { }; };

              _wrapper = mkOption { type = types.package; internal = true; };
              name = mkOption { type = types.str; internal = true; };
              type = mkOption { type = types.str; internal = true; };
              outputName = mkOption { type = types.str; internal = true; };
              drvPath = mkOption { type = types.path; internal = true; };
              outPath = mkOption { type = types.path; internal = true; };
            };
            config = {
              extraPackages = lib.optionals config.withAllTreesitGrammars [ "all-treesit-grammars" ];
              _wrapper =
                let
                  detectedPackages = gatherPackages cfg.initDirectory;
                  emacs-with-packages = (emacsPackages cfg.localPackages).emacsWithPackages (
                    epkgs: map (ename: epkgs.${ename}) (detectedPackages.elispPackages ++ cfg.extraPackages)
                  );

                in
                emacs-with-packages.overrideAttrs (super: {
                  deps = super.deps.overrideAttrs (dsuper:
                    let
                      genAutoloadsCommand = ''
                        echo "-- Generating autoloads..."
                        autoloads=$out/share/emacs/site-lisp/autoloads.el
                        for pkg in "''${requires[@]}"; do
                          autoload=("$pkg"/share/emacs/site-lisp/*/*/*-autoloads.el)
                          if [[ -e "$autoload" ]]; then
                            cat "$autoload" >> "$autoloads"
                          fi
                        done
                        echo "(load \"''$autoloads\")" >> "$siteStart"

                        # Byte-compiling improves start-up time only slightly, but costs nothing.
                        $emacs/bin/emacs --batch -f batch-byte-compile "$autoloads" "$siteStart"

                        $emacs/bin/emacs --batch \
                          --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
                          -f batch-native-compile "$autoloads" "$siteStart"
                      '';
                    in
                    {
                      buildCommand = ''
                        ${dsuper.buildCommand}
                        ${genAutoloadsCommand}
                      '';
                    });
                  buildCommand = ''
                    ${super.buildCommand}
                    wrapProgram $out/bin/emacs \
                                --append-flags "--init-directory ${toString cfg.initDirectory}"

                  '';
                });

              inherit (config._wrapper) name type outputName outPath drvPath;
            };
          });
        };
      };
    };

  lib' = import ./lib { inherit lib; };

  baseEmacsPackages = pkgs.emacsPackagesFor cfg.package;
  emacsPackages = extraEmacsPackages: baseEmacsPackages.overrideScope' (lib.composeManyExtensions [
    (final: prev: {
      mkPackage =
        { pname, src, files ? [ "*.el" ], ... }@args:

        let
          files' =
            let
              list = lib.concatStringsSep " " (map (f: ''"${lib.escape [''"''] f}"'') files);
            in
            "(${list})";
          version =
            let
              ver = src.lastModifiedDate or inputs.self.lastModifiedDate;
              removeLeadingZeros = s:
                let
                  s' = lib.removePrefix "0" s;
                in
                if lib.hasPrefix "0" s' then
                  removeLeadingZeros s'
                else
                  s'
              ;
              major = removeLeadingZeros (builtins.substring 0 8 ver);
              minor = removeLeadingZeros (builtins.substring 8 6 ver);
            in
            "${major}.${minor}";
        in
        final.melpaBuild ({
          inherit version src;
          commit = src.rev or inputs.self.sourceInfo.rev or inputs.self.sourceInfo.dirtyRev;
          recipe = pkgs.writeText "recipe" ''
            (${pname}
            :fetcher git
            :url "nohost.nodomain"
            :files ${files'})
          '';
        } // removeAttrs args [ "files" ]);

      all-treesit-grammars = final.treesit-grammars.with-grammars (grammars: lib.pipe grammars [
        (lib.filterAttrs (name: _: name != "recurseForDerivations"))
        builtins.attrValues
      ]);
    })
    (final: prev: lib.mapAttrs (pname: value: final.mkPackage ({ inherit pname; } // value)) extraEmacsPackages)
  ]);


  gatherPackages = initDir:
    let
      files = builtins.filter (f: lib.hasSuffix ".el" f) (lib.filesystem.listFilesRecursive initDir);
      fromElisp = import inputs.fromElisp { inherit pkgs; };
    in
    lib.zipAttrsWith (name: values: lib.flatten values)
      (map
        (f:
          let
            configText = builtins.readFile f;
          in
          lib'.parseSetup { inherit fromElisp; } configText)
        files);
in
self
