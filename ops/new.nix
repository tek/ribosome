{ self, config, lib }:
let

  nix = args: ''
    ${config.pkgs.nix}/bin/nix --option extra-substituters 'https://tek.cachix.org' \
    --option extra-trusted-public-keys 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=' ${args}
  '';

  script = config.pkgs.writeScript "ribosome-new" ''
    #!${config.pkgs.bash}/bin/bash
    set -e
    dir=$(${nix "run path:${self}#ribosome -- new --print-dir $*"})
    cd $dir
    ${nix "run .#gen"}
  '';

in script
