{
  description = "A Neovim plugin host and framework for Haskell";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    chiasma.url = git+https://git.tryp.io/tek/chiasma;
  };

  outputs = { self, hix, chiasma, ... }:
  let

    overrides = { hackage, configure, pkgs, buildInputs, jailbreak, notest, unbreak, ... }:
    let
      nvimBin = configure "--extra-prog-path=${pkgs.neovim}/bin";
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      chiasma = hackage "0.10.0.0" "1dmhrya6s323j2pvp51mi8x4vm6hmiiab2hcvh8m1vhyjkqvaiv3";
      criterion = notest;
      fuzzyfind = jailbreak (hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac");
      integration = inputs;
      ribosome = inputs;
      ribosome-host = nvimBin inputs;
      ribosome-menu = inputs;
      ribosome-test = inputs;
      streamly = hackage "0.8.2" "0jhsdd71kqw0k0aszg1qb1l0wbxl1r73hsmkdgch4vlx43snlc8a";
      type-errors = notest;
      type-errors-pretty = unbreak (notest jailbreak);
    };

  in hix.lib.pro ({ config, lib, ...}: {
    packages = {
      integration = ./packages/integration;
      ribosome = ./packages/ribosome;
      ribosome-app = ./packages/app;
      ribosome-host = ./packages/host;
      ribosome-host-test = ./packages/host-test;
      ribosome-menu = ./packages/menu;
      ribosome-test = ./packages/test;
    };
    main = "ribosome-menu";
    devGhc.compiler = "ghc902";
    inherit overrides;
    depsFull = [chiasma];
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage = {
      versionFile = "ops/version.nix";
      packages = ["ribosome-host" "ribosome-host-test" "ribosome" "ribosome-test" "ribosome-app"];
    };
    ghcid.shellConfig = {
      buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux pkgs.xterm];
      env = { RIBOSOME_ROOT = builtins.toPath self; };
    };
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels"];
    };
    compat.enable = false;
    outputs.apps = import ./ops/template-apps.nix { inherit self config lib; };
  }) // {
    templates = rec {
      default = plugin;
      plugin = {
        path = ./ops/template;
        description = "A Neovim plugin built with Ribosome";
      };
    };

    lib = hix.lib.extend (_: super: {
      modules = ms: super.modules (hix.inputs.nixpkgs.lib.toList ms ++ [(import ./ops/api.nix self)]);
    });
  };
}
