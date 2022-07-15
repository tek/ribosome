module Ribosome.App.Templates.Flake where

import Exon (exon)

import Ribosome.App.Data (FlakeUrl (FlakeUrl), ProjectName (ProjectName))

flakeNix :: FlakeUrl -> ProjectName -> Text
flakeNix (FlakeUrl flakeUrl) (ProjectName name) =
  [exon|{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = "#{flakeUrl}";
  };

  outputs = { ribosome, ... }: ribosome.lib.flake ({ config, lib, ... }: {
    base = ./.;
    packages.#{name} = ./packages/#{name};
    main = "#{name}";
    exe = "#{name}";
    depsFull = [ribosome];
    overrides = { buildInputs, pkgs, ... }: {
      #{name} = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    };
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
    };
  });
}
|]
