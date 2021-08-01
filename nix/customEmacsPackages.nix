{ writeText, inputs }:

final: prev:
{
  corfu = prev.corfu.override {
    elpaBuild = args: prev.melpaBuild (args // {
      src = inputs.corfu;
      commit = inputs.corfu.rev;
      recipe = writeText "recipe" ''
        (corfu
        :repo "minad/corfu"
        :fetcher github)
      '';
    });
  };
  embark = prev.embark.override {
    melpaBuild = args: prev.melpaBuild (args // {
      src = inputs.embark;
    });
  };
  vertico = prev.vertico.override {
    elpaBuild = args: prev.melpaBuild (args // {
      src = inputs.vertico;
      commit = inputs.vertico.rev;
      recipe = writeText "recipe" ''
        (vertico
        :repo "minad/vertico"
        :fetcher github)
      '';
    });
  };
  ligature = prev.trivialBuild {
    pname = "ligature";
    src = inputs.ligature;
  };
  evil-markdown = prev.melpaBuild {
    pname = "evil-markdown";
    version = "0.0.2";
    src = inputs.evil-markdown;
    commit = inputs.evil-markdown.rev;
    packageRequires = with final; [ evil ];
    recipe = writeText "recipe" ''
      (evil-markdown
      :repo "Somelauw/evil-markdown"
      :fetcher github
      :files ("evil-markdown.el"))
    '';
  };
}
