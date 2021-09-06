{ writeText, inputs }:

final: prev:
{
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
