{
  description = "A Neovim plugin host and framework for Haskell";

  inputs = {
    hix.url = github:tek/hix;
    chiasma.url = github:tek/chiasma/main;
    polysemy-conc.url = github:tek/polysemy-conc;
    polysemy-test.url = github:tek/polysemy-test;
    incipit.url = github:tek/incipit;
  };

  outputs = { self, hix, chiasma, polysemy-conc, polysemy-test, incipit, ... }:
  let
    RIBOSOME_ROOT = builtins.toPath self;

    overrides = { hackage, source, configure, pkgs, buildInputs, jailbreak, notest, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
    in {
      bytestring-trie = hackage "0.2.6" "0hlgdl7plif58r73hza2148671jf6l2pim84a0a7xf13n8bkrmh7";
      chiasma = source.package chiasma "chiasma";
      fuzzyfind = hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac";
      massiv = hackage "0.6.1.0" "133ixc95qw10ni54y4hrq7swq7bskf398s11zdakdvnj9v6hwlsr";
      scheduler = hackage "1.5.0" "143bsd0kfknrhdz37599k2didxmplljdpnf1ixmdfh4r9hnrp9f3";
      integration = inputs;
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      polysemy-process = source.package polysemy-conc "process";
      polysemy-test = source.package polysemy-test "polysemy-test";
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" inputs;
      ribosome-host = configure "--extra-prog-path=${pkgs.neovim}/bin" inputs;
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      type-errors = notest;
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
    depsFull = [incipit];
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
