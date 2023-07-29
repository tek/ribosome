{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
  };

  outputs = { ribosome, ... }: ribosome.lib.pro ({ config, lib, ... }: {
    main = "test-project";
    depsFull = [ribosome];
    hackage.versionFile = "ops/version.nix";
    genOverrides.enable = true;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "author";
      meta = {
        maintainer = "maintainer@home.page";
        category = "Neovim";
        github = "rep/org";
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

    packages.test-project = {
      src = ./packages/test-project;

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

    exe = "test-project";
    branch = "main";
    githubOrg = "org";
    githubRepo = "rep";
    cachixName = "cach";
    cachixKey = "12345";

    overrides = { buildInputs, pkgs, ... }: {
      test-project = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    };

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];

  });
}
