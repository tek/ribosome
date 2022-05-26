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
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Neovim";
    build-type = "Simple";
  };

  base = {
    name = "base";
    version = ">= 4.12 && < 5";
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

  project = name: merge (meta // { library = paths name; } // options) {
    inherit name;
    library = {
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

  ribosome-host = merge (project "ribosome-host") {
    synopsis = "Neovim plugin host for Polysemy";
    description = "See https://hackage.haskell.org/package/ribosome-host/docs/Ribosome-Host.html";
    library.dependencies = [
      "cereal"
      "exon"
      "flatparse"
      "lens"
      "messagepack"
      "path"
      "polysemy-conc"
      "polysemy-process"
      "template-haskell"
      "type-errors-pretty"
      "typed-process"
    ];
    tests = {
      ribosome-host-unit = exe "ribosome-host" "test" {
        dependencies = [
          "deepseq"
          "hedgehog"
          "messagepack"
          "ribosome-host"
          "polysemy-conc"
          "polysemy-test"
          "tasty"
          "time"
        ];
      };
    };
  };

  ribosome = merge (project "ribosome") {
    synopsis = "Neovim plugin framework for Polysemy";
    description = "See https://hackage.haskell.org/package/ribosome/docs/Ribosome.html";
    library.dependencies = [
      "aeson"
      "exon"
      "lens"
      "messagepack"
      "path"
      "path-io"
      "ribosome-host"
    ];
    tests = {
      ribosome-unit = exe "ribosome" "test" {
        dependencies = [
          "aeson"
          "hedgehog"
          "messagepack"
          "path"
          "ribosome"
          "ribosome-host"
          "polysemy-conc"
          "polysemy-test"
          "tasty"
          "time"
        ];
      };
    };
  };

  ribosome-menu = merge (project "ribosome-menu") {
    synopsis = "Menu widget for ribosome";
    description = "See https://hackage.haskell.org/package/ribosome-menu/docs/Ribosome-Menu.html";
    library.dependencies = [
      "bytestring-trie"
      "composition"
      "exceptions"
      "exon"
      "fuzzyfind"
      "lens"
      "lifted-base"
      "monad-control"
      "mtl"
      "ribosome"
      "ribosome-host"
      "stm-chans"
      "streamly"
      "transformers"
    ];
    tests.ribosome-menu-unit = exe "ribosome-menu" "test" {
      dependencies = [
        "hedgehog"
        "lens"
        "lifted-base"
        "ribosome"
        "ribosome-host"
        "ribosome-menu"
        "polysemy-conc"
        "polysemy-test"
        "streamly"
        "tasty"
        "time"
        "transformers"
      ];
    };
    benchmarks.ribosome-menu-bench = exe "ribosome-menu" "benchmark" {
      dependencies = [
        "criterion"
        "exon"
        "lens"
        "ribosome"
        "ribosome-menu"
        "path"
        "polysemy-conc"
        "polysemy-test"
        "streamly"
      ];
    };
  };

  integration = merge (project "integration") {
    synopsis = "Neovim plugin host for Polysemy";
    description = "Internal project for testing ribosome-host";
    library.dependencies = [
      "ribosome-host"
    ];
    tests = {
      integration = exe "integration" "test" {
        dependencies = [
          "exon"
          "hedgehog"
          "ribosome-host"
          "path"
          "path-io"
          "polysemy-conc"
          "polysemy-process"
          "polysemy-test"
          "tasty"
          "typed-process"
        ];
      };
    };
  };

}
