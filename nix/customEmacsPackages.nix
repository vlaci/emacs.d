{ lib, mu, writeText, inputs }:

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
  lsp-mode = prev.lsp-mode.overrideAttrs (super: {
    postPatch = ''
      substituteInPlace lsp-protocol.el \
        --replace '(getenv "LSP_USE_PLISTS")' 't'
    '';
  });
  ligature = build { pname = "ligature"; };
  eglot-x = build { pname = "eglot-x"; };
  explain-pause-mode = build { pname = "explain-pause-mode"; };
  bitwarden = build { pname = "bitwarden"; };
  inherit (final.melpaPackages) apheleia;
}
