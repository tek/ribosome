{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = "0.1.0.0";
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "NAME";
    maintainer = "EMAIL";
    copyright = "2022 NAME";
    category = "Neovim";
    build-type = "Simple";
    github = "OWNER/REPO";
  };

  base = {
    name = "base";
    version = ">= 4 && < 5";
    mixin = "hiding (Prelude)";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [base "incipit" "polysemy" "polysemy-plugin"];

  project = name: merge (meta // options) {
    inherit name;
    library = paths name // {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  plugin = merge (project "plugin") {
    synopsis = "A Neovim Plugin";
    description = "See https://hackage.haskell.org/package/PLUGIN/docs/PLUGIN.html";
    library.dependencies = [
      "ribosome"
      "ribosome-host"
      "ribosome-menu"
    ];
    executables.plugin = exe "plugin" "app" {
      dependencies = ["plugin"];
    };
    tests.plugin-unit = exe "plugin" "test" {
      dependencies = [
        "plugin"
        "polysemy-test"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "ribosome-test"
        "tasty"
      ];
    };
  };

}
