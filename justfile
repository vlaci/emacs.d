export NIXPKGS_ALLOW_UNFREE := "1"

# Prints help
help:
    just --list --unsorted

# Run fully complied package
run:
    $(nix build -L --impure --no-link --print-out-paths)/bin/emacs --debug-init

# Quickly try out  modifications
try:
    $(nix-build --no-out-link)/bin/emacs --debug-init
