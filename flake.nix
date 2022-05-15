{
  description = "A Neovim plugin host and framework for Haskell";

  inputs = {
    hix.url = github:tek/hix;
    chiasma.url = github:tek/chiasma/main;
    polysemy-conc.url = github:tek/polysemy-conc;
  };

  outputs = { self, hix, chiasma, polysemy-conc, ... }:
  let
    RIBOSOME_ROOT = builtins.toPath self;

    overrides = { hackage, source, configure, pkgs, buildInputs, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
    in {
      bytestring-trie = hackage "0.2.6" "0hlgdl7plif58r73hza2148671jf6l2pim84a0a7xf13n8bkrmh7";
      chiasma = source.package chiasma "chiasma";
      exon = hackage "0.3.0.0" "0jgpj8818nhwmb3271ixid38mx11illlslyi69s4m0ws138v6i18";
      flatparse = hackage "0.3.2.0" "01w71985b9ndg4wkfxqxjj7f1cynji6vp71akr7ivpmxn2drxspa";
      fuzzyfind = hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac";
      polysemy-process = source.package polysemy-conc "process";
      ribosome-host = configure "--extra-prog-path=${pkgs.neovim}/bin" inputs;
      integration = inputs;
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      unicode-data = hackage "0.2.0" "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
    };
  in hix.lib.flake ({ config, lib, ...}: {
    base = ./.;
    packages = {
      integration = ./packages/integration;
      ribosome = ./packages/ribosome;
      ribosome-host = ./packages/host;
      # ribosome-test = ./packages/test;
    };
    main = "ribosome-host";
    inherit overrides;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
    ghcid.shellConfig.env = { inherit RIBOSOME_ROOT; };
    ghci = {
      preludePackage = "incipit";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures"];
    };
    compat.enable = false;
  });
}
