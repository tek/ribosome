{ self, config, lib }:
let

  pkgs = config.devGhc.pkgs;

  nix = args: ''
    nix --option extra-substituters 'https://tek.cachix.org' \
    --option extra-trusted-public-keys 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=' ${args}
  '';

  script = pkgs.writeScript "ribosome-new" ''
    #!${pkgs.bash}/bin/bash
    set -e
    dir=$(${nix "run path:${self}#ribosome -- new --print-dir $*"})
    cd $dir
    ${nix "run .#hpack"}
  '';

in script
