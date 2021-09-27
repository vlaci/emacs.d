{ writeText, inputs }:

final: prev:
let
  getVersion = input:
    let
      ver = input.lastModifiedDate;
      major = builtins.substring 0 8 ver;
      minor = builtins.substring 9 5 ver;
    in "${major}.${minor}";
  build = { pname, files, ...}@args:
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
}
