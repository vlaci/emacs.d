{ mu, writeText, inputs }:

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
  build = { pname, files, ... }@args:
    let
      input = inputs.${pname};
    in final.melpaBuild ({
      src = input;
      commit = input.rev;
      version = getVersion input;
      recipe = writeText "recipe" ''
        (${pname}
        :repo ""
        :fetcher github
        :files ${files})
      '';
    } // removeAttrs args [ "files" ]);
in {
  ligature = build { pname = "ligature"; files = ''("ligature.el")''; };
  evil-markdown = build { pname = "evil-markdown"; files = ''("evil-markdown.el")''; };
  org = build {
    pname = "org";
    files = ''("lisp/*.el")'';
    preBuild = ''
      # dummy .git direcotry to force org into proper version detection
      mkdir .git
      # this will create the necessary auxulary files like org-version.el
      make autoloads
    '';
  };
  mu4e-thread-folding = build {
    pname = "mu4e-thread-folding";
    files = ''("mu4e-thread-folding.el")'';
  };
}
