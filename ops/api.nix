self: { config, lib, ... }:
with lib;
{
  options = with types; {

    exe = mkOption {
      type = str;
      description = "The name of the package building the Neovim plugin's executable.";
    };

    branch = mkOption {
      type = str;
      description = "The name of the main branch.";
      default = "master";
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

    outputs.apps.boot = {
      type = "app";
      program = "${import ./boot.nix { inherit self config; }}";
    };

    outputs.packages.plugin = let

      name = config.exe;

      dir = config.outputs.packages.${name}.min;

      plugin = config.pkgs.writeTextFile {
        inherit name;
        destination = "/plugin/${name}.vim";
        text = ''
        let s:args = get(g:, '${name}_cli_args', [])
        let s:opts = { 'rpc': v:true, 'cwd': '${dir}', }
        call jobstart(['${dir}/bin/${name}'] + s:args, s:opts)
        '';
      };

    in config.pkgs.vimUtils.toVimPlugin plugin;

  };
}
