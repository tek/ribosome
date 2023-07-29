module Ribosome.App.Templates.Flake where

import Exon (exon)

import Ribosome.App.Data (
  Author,
  Branch (Branch),
  Cachix (Cachix),
  CachixKey (CachixKey),
  CachixName (CachixName),
  FlakeUrl (FlakeUrl),
  Github (Github),
  GithubOrg (GithubOrg),
  GithubRepo (GithubRepo),
  Maintainer,
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

githubField ::
  Github ->
  Text
githubField (Github (GithubOrg org) (GithubRepo repo)) =
  [exon|
        github = "#{repo}/#{org}";|]

flakeNix ::
  FlakeUrl ->
  ProjectName ->
  Author ->
  Maintainer ->
  Branch ->
  Maybe Github ->
  Maybe Cachix ->
  Text
flakeNix (FlakeUrl flakeUrl) (ProjectName name) author maintainer (Branch branch) github cachix =
  [exon|{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = "#{flakeUrl}";
  };

  outputs = { ribosome, ... }: ribosome.lib.pro ({ config, lib, ... }: {
    main = "#{name}";
    depsFull = [ribosome];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "##{author}";
      meta = {
        maintainer = "##{maintainer}";
        category = "Neovim";#{foldMap githubField github}
      };
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.6";
        };
        module = "Prelate";
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    packages.#{name} = {
      src = ./packages/#{name};

      cabal.meta.synopsis = "A Neovim Plugin";

      library = {
        enable = true;
        dependencies = [
          "ribosome"
          "ribosome-host"
          "ribosome-menu"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "polysemy-test"
          "ribosome"
          "ribosome-host"
          "ribosome-host-test"
          "ribosome-test"
          "tasty"
        ];
      };

      executable.enable = true;

    };

    exe = "#{name}";
    branch = "#{branch}";#{foldMap githubAttrs github}#{foldMap cachixAttrs cachix}

    buildInputs = pkgs: [pkgs.neovim pkgs.tmux pkgs.xterm];

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];

  });
}
|]
