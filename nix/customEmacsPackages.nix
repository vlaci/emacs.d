{ lib, enchant, fetchpatch, writeText, inputs }:

final: prev:
let
  getVersion = input:
    let
      ver = input.lastModifiedDate;
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
  build = { pname, files ? [ "*.el" ], ... }@args:
    let
      input = inputs.${pname};
      files' =
        let
          list = lib.concatStringsSep " " (map (f: ''"${lib.escape [''"''] f}"'') files);
        in
        "(${list})";
    in
    final.melpaBuild ({
      src = input;
      commit = input.rev;
      version = getVersion input;
      recipe = writeText "recipe" ''
        (${pname}
        :repo ""
        :fetcher github
        :files ${files'})
      '';
    } // removeAttrs args [ "files" ]);
in
{
  ligature = build { pname = "ligature"; };
  eglot-x = build { pname = "eglot-x"; };
  explain-pause-mode = build { pname = "explain-pause-mode"; };
  bitwarden = build { pname = "bitwarden"; };
  combobulate = build { pname = "combobulate"; };
  nushell-mode = build { pname = "nushell-mode"; };
  evil-quick-diff = build { pname = "evil-quick-diff"; };
  evil-textobj-tree-sitter = build {
    pname = "evil-textobj-tree-sitter";
    files = [ "*.el" "queries" "treesit-queries" ];
  };

  general = prev.general.overrideAttrs (super: {
    patches = [ ./general.patch ];
  });
  jinx = prev.jinx.overrideAttrs (super: {
    buildInputs = [ enchant ];
    preBuild = ''
      cc -I -O2 -Wall -Wextra -fPIC -shared -I${enchant.dev}/include/enchant-2 -lenchant-2 -o $NIX_BUILD_TOP/jinx-mod.so jinx-mod.c
    '';
    postInstall = ''
      install -m 444 jinx-mod.so $out/share/emacs/site-lisp/elpa/jinx-*
    '';
  });

  inherit (final.melpaPackages) apheleia;
  inherit (final.nongnuPackages) eat;
  eglot = null;
}
