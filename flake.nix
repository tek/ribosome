{
  description = "A Neovim plugin host and framework for Haskell";

  inputs = {
    hix.url = github:tek/hix;
    chiasma.url = git+https://git.tryp.io/tek/chiasma?ref=main;
    polysemy-conc.url = github:tek/polysemy-conc;
    prelate.url = git+https://git.tryp.io/tek/prelate;
  };

  outputs = { self, hix, chiasma, polysemy-conc, prelate, ... }:
  let
    RIBOSOME_ROOT = builtins.toPath self;

    overrides = { hackage, source, configure, pkgs, buildInputs, jailbreak, notest, ... }:
    let
      nvimBin = configure "--extra-prog-path=${pkgs.neovim}/bin";
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      bytestring-trie = hackage "0.2.6" "0hlgdl7plif58r73hza2148671jf6l2pim84a0a7xf13n8bkrmh7";
      fuzzyfind = hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac";
      massiv = hackage "0.6.1.0" "133ixc95qw10ni54y4hrq7swq7bskf398s11zdakdvnj9v6hwlsr";
      scheduler = hackage "1.5.0" "143bsd0kfknrhdz37599k2didxmplljdpnf1ixmdfh4r9hnrp9f3";
      integration = inputs;
      polysemy-conc = source.package polysemy-conc "conc";
      polysemy-process = source.package polysemy-conc "process";
      prelate = source.package prelate "prelate";
      ribosome = inputs;
      ribosome-host = nvimBin inputs;
      ribosome-menu = inputs;
      ribosome-test = inputs;
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      type-errors-pretty = notest jailbreak;
      unicode-data = hackage "0.2.0" "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
    };
  in hix.lib.flake ({ config, lib, ...}: {
    base = ./.;
    packages = {
      integration = ./packages/integration;
      ribosome = ./packages/ribosome;
      ribosome-host = ./packages/host;
      ribosome-host-test = ./packages/host-test;
      ribosome-menu = ./packages/menu;
      ribosome-test = ./packages/test;
    };
    main = "ribosome-menu";
    inherit overrides;
    depsFull = [chiasma];
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig = {
      buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux pkgs.xterm];
      env = { inherit RIBOSOME_ROOT; };
    };
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels"];
    };
    compat.enable = false;
  }) // {
    templates = rec {
      default = plugin;
      plugin = {
        path = ./ops/template;
        description = "A Neovim plugin built with Ribosome";
      };
    };
  };
}
