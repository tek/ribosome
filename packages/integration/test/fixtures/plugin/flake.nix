{
  description = "Test plugin";

  inputs.ribosome.url = "path:RIBOSOME";

  outputs = { ribosome, ... }:
  ribosome.inputs.hix.lib.pro {
    depsFull = [ribosome];
    # localPackage = api: api.fast;
    overrides = { fast, ... }: {
      ribosome-host = fast;
      ribosome-host-test = fast;
      ribosome = fast;
      ribosome-test = fast;
      integration = fast;
    };
    packages.test-plugin = { src = ./packages/test-plugin; };
    ifd = true;
  };
}
