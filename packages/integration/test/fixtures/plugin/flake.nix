{
  description = "Test plugin";

  inputs.ribosome.url = "path:RIBOSOME";

  outputs = { ribosome, ... }:
  ribosome.inputs.hix.lib.pro {
    depsFull = [ribosome];
    envs.dev.localPackage = api: api.fast;
    packages.test-plugin = { src = ./packages/test-plugin; };
    ifd = true;
  };
}
