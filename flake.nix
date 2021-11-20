{
  description = "High Level Neovim Plugin Framework";

  inputs.hix.url = github:tek/hix;
  inputs.chiasma.url = github:tek/chiasma;

  outputs = { hix, chiasma, ... }:
  let
    overrides = { hackage, source, configure, pkgs, transform_, ... }: {
      chiasma = source.package chiasma "chiasma";
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      fuzzyfind = hackage "3.0.0" "1aba9rxxdi6sv0z6qgwyq87fnqqhncqakvrbph0fvppd0lnajaac";
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin";
      ribosome-test = transform_ (drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
      }));
    };
  in hix.flake {
    base = ./.;
    packages = {
      ribosome = ./packages/ribosome;
      ribosome-test = ./packages/test;
    };
    main = "ribosome-test";
    inherit overrides;
    versionFile = "ops/hpack/shared/meta.yaml";
    shellConfig = p: { buildInputs = [p.pkgs.neovim]; };
    compat = false;
  };
}
