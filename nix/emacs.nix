{ inputs
, emacsWithPackagesFromUsePackage
, emacs
, runCommand
, writeText
, vscode-extensions
, clang-tools
, hunspellDicts
, hunspellWithDicts
, jre
, languagetool
, nodejs-slim
, nodePackages
, sumneko-lua-language-server
}:

let
  emacs-nixos-integration =
    let
      hunspell = hunspellWithDicts (with hunspellDicts; [ hu-hu en-us ]);
      cpptools = runCommand "cpptools"
        {
          cpptools = vscode-extensions.ms-vscode.cpptools;
        } ''
        mkdir -p $out
        ln -snf $cpptools/share/vscode/extensions/ms-vscode.cpptools $out/extension
      '';
    in
    writeText "nixos-integration.el" ''
      (setq-default ispell-program-name "${hunspell}/bin/hunspell")
      (setq-default langtool-java-bin "${jre}/bin/java"
                    langtool-language-tool-jar "${languagetool}/share/languagetool-commandline.jar")
      (setq-default dap-cpptools-debug-path "${cpptools}"
                    lsp-eslint-server-command (list "${nodejs-slim}/bin/node" "${vscode-extensions.dbaeumer.vscode-eslint}/share/vscode/extensions/dbaeumer.vscode-eslint/server/out/eslintServer.js" "--stdio")
                    lsp-clangd-binary-path "${clang-tools}/bin/clangd"
                    lsp-clients-typescript-tls-path "${nodePackages.typescript-language-server}/bin/typescript-language-server"
                    lsp-clients-lua-language-server-bin "${sumneko-lua-language-server}/bin/lua-language-server"
                    lsp-clients-lua-language-server-main-location "${sumneko-lua-language-server}/extras/main.lua")

       (advice-add 'lsp-css--server-command
                   :override (lambda () (list "${nodePackages.vscode-css-languageserver-bin}/bin/css-languageserver" "--stdio")))
    '';
  emacs_d = runCommand "mk-emacs.d"
    {
      buildInputs = [ emacs ];
      literateInitFile = ../README.org;
    } ''
    mkdir -p $out
    cp $literateInitFile $out/init.org
    emacs --batch --quick -l ob-tangle --eval '(org-babel-tangle-file "'$out'/init.org")'
    rm $out/init.org
  '';

in
emacsWithPackagesFromUsePackage
  {
    config = ../README.org;
    package = emacs;
    alwaysEnsure = true;
    alwaysTangle = true;

    extraEmacsPackages = epkgs: with epkgs; let
      default = epkgs.trivialBuild {
        pname = "default";
        packageRequires = [
          use-package
        ];
        buildPhase = ":";
        postInstall = ":";
        unpackPhase = ''
          cp ${emacs-nixos-integration} ./nixos-integration.el
          cp ${emacs_d}/*.el .
          cat > default.el <<EOF
          (load "nixos-integration")
          (load "init")
          EOF
        '';
      };
    in
    [
      default
    ];
  } // {
  inherit emacs_d emacs-nixos-integration;
}
