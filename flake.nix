{
  description = "High Level Neovim Plugin Framework";

  inputs.hix.url = github:tek/hix;
  inputs.chiasma.url = github:tek/chiasma;

  outputs = { hix, chiasma, ... }:
  let
    overrides = { hackage, source, only, configure, pkgs, ... }: {
      chiasma = source.package chiasma "chiasma";
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin";
      ribosome-test = drv: drv.overrideAttrs (old: { buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux]; });
    };
  in hix.flake {
    base = ./.;
    packages = {
      ribosome = ./packages/ribosome;
      ribosome-test = ./packages/test;
    };
    main = "ribosome-test";
    inherit overrides;
    compatOverrides = overrides;
    versionFile = "ops/hpack/shared/meta.yaml";
    runConfig = p: {
      extraShellInputs = [p.pkgs.neovim];
    };
    ghcid.easy-hls = false;
  };
}
