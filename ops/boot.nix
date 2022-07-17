{ self, config, ... }:
let

  githubOrg =
    if config.githubOrg == null
    then ""
    else "--github-org ${config.githubOrg}";

  githubRepo =
    if config.githubRepo == null
    then ""
    else "--github-repo ${config.githubRepo}";

  cachixName =
    if config.cachixName == null
    then ""
    else "--cachix ${config.cachixName}";

  cachixKey =
    if config.cachixKey == null
    then ""
    else "--cachix-key ${config.cachixKey}";

  script = config.pkgs.writeScript "ribosome-regen-boot" ''
    #!${config.pkgs.zsh}/bin/zsh
    nix run path:${self}#ribosome -- boot ${config.exe} ${githubOrg} ${githubRepo} ${cachixName} ${cachixKey} "$@"
  '';

in script
