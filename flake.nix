{
  description = "High Level Neovim Plugin Framework";

  inputs.hix.url = github:tek/hix;
  inputs.chiasma.url = github:tek/chiasma;

  outputs = { hix, chiasma, ... }:
  let
    overrides = { hackage, source, configure, pkgs, transform_, unbreak, ... }: {
      bytestring-trie = hackage "0.2.6" "0hlgdl7plif58r73hza2148671jf6l2pim84a0a7xf13n8bkrmh7";
      chiasma = source.package chiasma "chiasma";
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      exon = hackage "0.2.0.1" "0hs0xrh1v64l1n4zqx3rqfjdh6czxm7av85kj1awya9zxcfcy5cl";
      flatparse = unbreak;
      fuzzyfind = hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac";
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin";
      ribosome-test = transform_ (drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
      }));
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      unicode-data = hackage "0.2.0" "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
    };
  in hix.lib.flake ({ config, ...}: {
    base = ./.;
    packages = {
      ribosome = ./packages/ribosome;
      ribosome-test = ./packages/test;
    };
    main = "ribosome-test";
    inherit overrides;
    versionFile = "ops/hpack/shared/meta.yaml";
    ghcid.shellConfig =
    let pkgs = config.devGhc.pkgs;
    in { buildInputs = [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode]; };
    compat.enable = false;
  });
}
