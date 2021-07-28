# Print help
help:
    just --list --unsorted

try:
    #!/bin/sh
    OUT=$(mktemp -u --suffix .result)
    trap "rm -f $OUT" EXIT
    NIXPKGS_ALLOW_UNFREE=1 nix build --impure --out-link $OUT
    $OUT/bin/emacs

tangle:
    #!/usr/bin/env nix-shell
    #!nix-shell -p emacs -i "emacs --script"
    (require 'ob-tangle)
    (org-babel-tangle-file "README.org" nil "just\\|sh")
