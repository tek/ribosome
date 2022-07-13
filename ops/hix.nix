{ config, lib, ... }:
with lib;
with types;
let
  overrides = { self, hsLib, ... }: {
    static = hsLib.justStaticExecutables self.${config.exe};
  };
in {
  options = {
    exe = mkOption {
      type = str;
      description = "The name of the package building the Neovim plugin's executable";
    };
  };

  config = {
    exe = mkDefault config.main;

    extraOverrides = {
      all = [overrides];
    };

    output.extraPackages = ["static"];
  };
}
