{ self, config, lib }: {

  template-test = {
    type = "app";
    program = "${import ./template-test.nix { inherit self config lib; }}";
  };

  new = {
    type = "app";
    program = "${import ./new.nix { inherit self config lib; }}";
  };

}
