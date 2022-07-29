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
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Neovim";
    build-type = "Simple";
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

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "prelate"; version = ">= 0.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
      "polysemy"
      "polysemy-plugin"
    ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = pkg: dir: merge (paths pkg // {
    main = "Main.hs";
    source-dirs = dir;
    dependencies = dependencies ++ [pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  ribosome-host = merge (project "ribosome-host" "Ribosome-Host") {
    synopsis = "Neovim plugin host for Polysemy";
    library.dependencies = [
      "aeson >= 2"
      "casing"
      "cereal"
      "chronos"
      "exon"
      "first-class-families"
      "flatparse"
      "generics-sop"
      "messagepack"
      "network"
      "optparse-applicative"
      "path"
      "polysemy-chronos"
      "polysemy-log"
      "polysemy-process"
      "template-haskell"
      "type-errors-pretty"
      "typed-process"
    ];
    tests.ribosome-host-unit = exe "ribosome-host" "test" {
      dependencies = [
        "chronos"
        "deepseq"
        "exon"
        "generic-lens"
        "hedgehog"
        "messagepack"
        "optparse-applicative"
        "path"
        "polysemy-chronos"
        "polysemy-conc"
        "polysemy-test"
        "tasty"
      ];
    };
  };

  ribosome-host-test = merge (project "ribosome-host-test" "Ribosome-Host-Test") {
    synopsis = "Test tools for Ribosome";
    library.dependencies = [
      "chronos"
      "hedgehog"
      "ribosome-host"
      "polysemy-chronos"
      "polysemy-test"
    ];
  };

  ribosome = merge (project "ribosome" "Ribosome") {
    synopsis = "Neovim plugin framework for Polysemy";
    library.dependencies = [
      "aeson"
      "exon"
      "generic-lens"
      "messagepack"
      "optparse-applicative"
      "path"
      "path-io"
      "polysemy-chronos"
      "prettyprinter"
      "rainbow"
      "ribosome-host"
    ];
    tests.ribosome-unit = exe "ribosome" "test" {
      dependencies = [
        "chronos"
        "exon"
        "hedgehog"
        "messagepack"
        "path"
        "ribosome-host"
        "ribosome-host-test"
        "polysemy-conc"
        "polysemy-test"
        "tasty"
      ];
    };
    executables.ribosome = exe "ribosome" "app" {};
  };

  ribosome-test = merge (project "ribosome-test" "Ribosome-Test") {
    synopsis = "Test tools for Ribosome";
    library.dependencies = [
      "chiasma"
      "chiasma-test"
      "exon"
      "generic-lens"
      "hedgehog"
      "lens-regex-pcre"
      "path"
      "path-io"
      "polysemy-chronos"
      "polysemy-test"
      "ribosome"
      "ribosome-host"
      "ribosome-host-test"
    ];
    tests.ribosome-test-unit = exe "ribosome-test" "test" {
      dependencies = [
        "polysemy-test"
        "ribosome"
        "ribosome-host"
        "tasty"
      ];
    };
  };

  ribosome-menu = merge (project "ribosome-menu" "Ribosome-Menu") {
    synopsis = "Menu widget for Ribosome";
    library.dependencies = [
      "bytestring-trie"
      "exon"
      "fuzzyfind"
      "generic-lens"
      "microlens-mtl"
      "mtl"
      "polysemy-chronos"
      "ribosome"
      "ribosome-host"
      "streamly"
      "transformers"
    ];
    tests.ribosome-menu-unit = exe "ribosome-menu" "test" {
      dependencies = [
        "hedgehog"
        "microlens-mtl"
        "polysemy-chronos"
        "polysemy-test"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "ribosome-test"
        "streamly"
        "tasty"
        "tasty-hedgehog"
        "transformers"
      ];
    };
    benchmarks.ribosome-menu-bench = exe "ribosome-menu" "benchmark" {
      dependencies = [
        "criterion"
        "exon"
        "microlens"
        "ribosome"
        "path"
        "polysemy-conc"
        "polysemy-test"
        "streamly"
      ];
    };
  };

  integration = merge (project "integration" "") {
    synopsis = "Neovim plugin host for Polysemy";
    description = "Internal project for testing Ribosome";
    library.dependencies = [
        "messagepack"
        "ribosome"
        "ribosome-host"
    ];
    tests.integration = exe "integration" "test" {
      dependencies = [
        "exon"
        "hedgehog"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "ribosome-test"
        "path"
        "path-io"
        "polysemy-chronos"
        "polysemy-conc"
        "polysemy-process"
        "polysemy-test"
        "tasty"
        "typed-process"
      ];
    };
  };

}
