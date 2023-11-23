{ lib }:

{
  parseSetup =
    # Parse configuration that uses setup.el <https://git.sr.ht/~pkal/setup>
    {
      fromElisp,
      packageKeyword ? ":package",
      nixpkgsKeyword ? ":nixpkgs",
    }:
    with builtins;
    string: let
      setups = lib.pipe (fromElisp.fromElisp string) [
        (filter (block: head block == "setup"))
        (map tail)
      ];
    
      collect = keyword:
      with builtins;
      let
        go = {
          data,
          rest,
        } @ acc: fields: {
          data =
            data
            ++ lib.pipe fields [
              (map (field:
              if isList field && length field > 0 && head field == keyword
              then tail field
              else null))
              (filter lib.isList)
            ];
            rest =
              (lib.pipe fields [
                (filter isList)
                concatLists
                (filter isList)
              ])
              ++ rest;
        };
    
        recurse = {rest, ...} @ acc:
        if rest == []
        then removeAttrs acc ["rest"]
        else recurse (go (acc // {rest = [];}) rest);
      in
      blocks:
      lib.pipe blocks [
        (foldl' go {
          data = [];
          rest = [];
        })
        recurse
        (attrs: attrs.data)
      ];
    in {
      elispPackages = lib.pipe (collect packageKeyword setups) [
        lib.concatLists
        lib.unique
      ];
    
      nixPackages = lib.pipe (collect nixpkgsKeyword setups) [
          lib.concatLists
          lib.unique
        ];
      }
    ;
} // lib
