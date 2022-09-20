module Ribosome.App.Templates.Flake where

import Exon (exon)

import Ribosome.App.Data (
  Branch (Branch),
  Cachix (Cachix),
  CachixKey (CachixKey),
  CachixName (CachixName),
  FlakeUrl (FlakeUrl),
  Github (Github),
  GithubOrg (GithubOrg),
  GithubRepo (GithubRepo),
  ProjectName (ProjectName),
  )

githubAttrs :: Github -> Text
githubAttrs (Github (GithubOrg org) (GithubRepo repo)) =
  [exon|
    githubOrg = "#{org}";
    githubRepo = "#{repo}";|]

cachixAttrs :: Cachix -> Text
cachixAttrs (Cachix (CachixName name) (CachixKey key)) =
  [exon|
    cachixName = "#{name}";
    cachixKey = "#{key}";|]

flakeNix ::
  FlakeUrl ->
  ProjectName ->
  Branch ->
  Maybe Github ->
  Maybe Cachix ->
  Text
flakeNix (FlakeUrl flakeUrl) (ProjectName name) (Branch branch) github cachix =
  [exon|{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = "#{flakeUrl}";
  };

  outputs = { ribosome, ... }: ribosome.lib.pro ({ config, lib, ... }: {
    base = ./.;
    packages.#{name} = ./packages/#{name};
    main = "#{name}";
    exe = "#{name}";
    branch = "#{branch}";#{foldMap githubAttrs github}#{foldMap cachixAttrs cachix}
    depsFull = [ribosome];
    devGhc.compiler = "ghc902";
    overrides = { buildInputs, pkgs, ... }: {
      #{name} = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    };
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
    };
  });
}
|]
