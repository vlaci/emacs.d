# Print help
help:
    just --list --unsorted

try:
    #!/bin/sh -e
    OUT=$(mktemp -u --suffix .result)
    echo "Building into $OUT..."
    trap "rm -f $OUT" EXIT
    NIXPKGS_ALLOW_UNFREE=1 nix build --impure --out-link $OUT
    $OUT/bin/emacs --debug-init

tangle:
    #!/usr/bin/env nix-shell
    #!nix-shell -p emacs -i "emacs --script"
    (require 'ob-tangle)
    (org-babel-tangle-file "README.org" nil)
