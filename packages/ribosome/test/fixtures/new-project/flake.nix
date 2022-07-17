{
  description = "A Neovim Plugin";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
  };

  outputs = { ribosome, ... }: ribosome.lib.flake ({ config, lib, ... }: {
    base = ./.;
    packages.test-project = ./packages/test-project;
    main = "test-project";
    exe = "test-project";
    branch = "main";
    githubOrg = "org";
    githubRepo = "rep";
    cachixName = "cach";
    cachixKey = "12345";
    depsFull = [ribosome];
    overrides = { buildInputs, pkgs, ... }: {
      test-project = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
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
