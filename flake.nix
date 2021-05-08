{
  description = "tmux api";

  inputs = {
    hix.url = github:tek/hix;
    chiasma.url = github:tek/chiasma;
  };

  outputs = { hix, chiasma, ... }:
  let
    overrides = { hackage, source, only, configure, pkgs, ... }: {
      chiasma = source.package chiasma "chiasma";
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      nvim-hs = only "8.6.5" (hackage "2.1.0.4" "1fpjrfgmbmkd2qi9mjfjkidgcf8cbqzpaqw36zy2fznm3qkhpss7");
      unliftio-core = only "8.6.5" (hackage "0.2.0.1" "06cbv2yx5a6qj4p1w91q299r0yxv96ms72xmjvkpm9ic06ikvzzq");
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin";
      posix-pty = only "8.6.5" (hackage "0.2.1.1" "0rwb7fj7134lc04dv9vn4j6nq0vadl2qhzlz63pd0an9vqmxw685");
    };
  in hix.flake {
    base = ./.;
    packages = {
      ribosome = ./packages/ribosome;
      ribosome-test = ./packages/ribosome-test;
    };
    main = "ribosome-test";
    inherit overrides;
    compatOverrides = overrides;
    versionFile = "ops/hpack/shared/meta.yaml";
    runConfig = p: {
      extraShellInputs = [p.pkgs.neovim];
    };
  };
}
