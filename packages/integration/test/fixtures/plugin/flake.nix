{
  description = "Test plugin";

  inputs.ribosome.url = path:RIBOSOME;

  outputs = { ribosome, ... }:
  ribosome.inputs.hix.lib.flake {
    base = ./.;
    depsFull = [ribosome];
    overrides = { fast, ... }: {
      ribosome-host = fast;
    };
    packages.test-plugin = ./packages/test-plugin;
  };
}
