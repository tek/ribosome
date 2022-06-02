{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = git+https://gitlab.tryp.io/haskell/ribosome?ref=polysemy;
  };

  outputs = { ribosome, ... }:
  let
    inherit (ribosome.inputs) hix;

    overrides = { buildInputs, pkgs, ... }: {
      plugin = buildInputs [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
    };

  in hix.lib.flake ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    packages.plugin = ./packages/plugin;
    depsFull = [ribosome];
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "incipit";
      args = ["-fplugin=Polysemy.Plugin"];
    };
    output.amend = _: outputs: rec {
      apps = rec {
        plugin = {
          type = "app";
          program = "${outputs.packages.plugin}/bin/plugin";
        };
        default = plugin;
      };
    };
  });
}
