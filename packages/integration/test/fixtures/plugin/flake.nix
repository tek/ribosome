{
  description = "Test plugin";

  inputs.ribosome.url = path:RIBOSOME;

  outputs = { ribosome, ... }:
  ribosome.inputs.hix.lib.flake {
    base = ./.;
    depsFull = [ribosome];
    packages.test-plugin = ./packages/test-plugin;
  };
}
