{ config, lib, ... }:
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
    };

    githubRepo = mkOption {
      type = str;
      description = ''
        The Github repository name, used for generating the download link for the binary.
        Defaults to the main executable name.
      '';
    };

  };

  config = {

    exe = mkDefault config.main;

    githubRepo = mkDefault config.exe;

    extraOverrides = {
      all = [overrides];
    };

    output.extraPackages = ["static"];

  };
}
