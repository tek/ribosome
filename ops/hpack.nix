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

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "incipit"; version = ">= 0.3"; mixin = ["(Incipit as Prelude)" "hiding (Incipit)"]; }
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

  ribosome-host = merge (project "ribosome-host" "Ribosome-Host") {
    synopsis = "Neovim plugin host for Polysemy";
    library.dependencies = [
      "cereal"
      "chronos"
      "exon"
      "flatparse"
      "generics-sop"
      "lens"
      "messagepack"
      "network"
      "path"
      "polysemy-chronos"
      "polysemy-conc"
      "polysemy-log"
      "polysemy-process"
      "template-haskell"
      "time"
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
        "lens"
        "messagepack"
        "path"
        "polysemy-chronos"
        "polysemy-conc"
        "polysemy-test"
        "ribosome-host"
        "tasty"
        "time"
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
      "lens"
      "messagepack"
      "path"
      "path-io"
      "polysemy-chronos"
      "prettyprinter"
      "ribosome-host"
    ];
    tests.ribosome-unit = exe "ribosome" "test" {
      dependencies = [
        "aeson"
        "hedgehog"
        "messagepack"
        "path"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "polysemy-conc"
        "polysemy-test"
        "tasty"
      ];
    };
  };

  ribosome-test = merge (project "ribosome-test" "Ribosome-Test") {
    synopsis = "Test tools for Ribosome";
    library.dependencies = [
      "cereal"
      "chiasma"
      "chiasma-test"
      "exon"
      "generic-lens"
      "hedgehog"
      "lens"
      "lens-regex-pcre"
      "messagepack"
      "network"
      "ribosome"
      "ribosome-host"
      "ribosome-host-test"
      "path"
      "path-io"
      "polysemy-test"
      "polysemy-chronos"
      "polysemy-process"
      "typed-process"
    ];
    tests.ribosome-test-unit = exe "ribosome-test" "test" {
      dependencies = [
        "aeson"
        "chiasma"
        "chiasma-test"
        "hedgehog"
        "messagepack"
        "path"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "ribosome-test"
        "polysemy-conc"
        "polysemy-test"
        "tasty"
      ];
    };
  };

  ribosome-menu = merge (project "ribosome-menu" "Ribosome-Menu") {
    synopsis = "Menu widget for Ribosome";
    library.dependencies = [
      "bytestring-trie"
      "composition"
      "exceptions"
      "exon"
      "fuzzyfind"
      "generic-lens"
      "lens"
      "lifted-base"
      "monad-control"
      "mtl"
      "polysemy-conc"
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
        "ribosome-host-test"
        "ribosome-menu"
        "ribosome-test"
        "polysemy-conc"
        "polysemy-test"
        "streamly"
        "tasty"
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

  integration = merge (project "integration" "") {
    synopsis = "Neovim plugin host for Polysemy";
    description = "Internal project for testing Ribosome";
    library.dependencies = [
        "exon"
        "hedgehog"
        "messagepack"
        "ribosome"
        "ribosome-host"
        "path"
        "path-io"
        "polysemy-chronos"
        "polysemy-conc"
        "polysemy-process"
        "polysemy-test"
        "tasty"
        "typed-process"
    ];
    tests.integration = exe "integration" "test" {
      dependencies = [
        "exon"
        "hedgehog"
        "integration"
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
