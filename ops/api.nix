self: { config, lib, ... }:
with lib;
let
  overrides = { self, hsLib, ... }: {
    static = hsLib.justStaticExecutables self.${config.exe};
  };

in {
  options = with types; {

    exe = mkOption {
      type = str;
      description = "The name of the package building the Neovim plugin's executable.";
    };

    githubOrg = mkOption {
      type = nullOr str;
      description = ''
        The Github organization, used for generating the download link for the binary.
      '';
      default = null;
    };

    githubRepo = mkOption {
      type = str;
      description = ''
        The Github repository name, used for generating the download link for the binary.
        Defaults to the main executable name.
      '';
    };

    cachixName = mkOption {
      type = nullOr str;
      description = ''
        The name of a Cachix cache to use, for minimizing build times in Github Actions as well as for users building
        with Nix.
      '';
      default = null;
    };

    cachixKey = mkOption {
      type = nullOr str;
      description = ''
        The public key for the Cachix cache, if used.
      '';
      default = null;
    };

  };

  config = {

    exe = mkDefault config.main;

    githubRepo = mkDefault config.exe;

    extraOverrides = {
      all = [overrides];
    };

    output.extraPackages = ["static"];

    outputs.apps.boot = {
      type = "app";
      program = "${import ./boot.nix {inherit self config; }}";
    };

  };
}
