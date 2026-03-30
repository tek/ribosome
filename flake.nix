{
  description = "A Neovim plugin host and framework for Haskell";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {self, hix, ...}: hix.lib.pro ({config, lib, ...}: {
    compiler = "ghc912";
    ghcVersions = lib.mkForce [];
    main = "ribosome-menu";
    compat.enable = false;
    gen-overrides.enable = true;
    hackage = {
      versionFile = "ops/version.nix";
      packages = ["ribosome-host" "ribosome-host-test" "ribosome" "ribosome-test" "ribosome-app"];
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      language = "GHC2021";
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Neovim";
        github = "tek/ribosome";
        extra-source-files = ["readme.md" "changelog.md"];
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = ">= 0.6 && < 0.9";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy ^>= 1.9" "polysemy-plugin ^>= 0.4"];
    };

    overrides = {jailbreak, force, unbreak, notest, source, hackage, ...}: {
      chiasma = hackage "0.11.0.0" "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
      chiasma-test = hackage "0.11.0.0" "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
      exon = jailbreak;
      fuzzyfind = force;
      incipit = jailbreak;
      incipit-base = jailbreak;
      incipit-core = jailbreak;
      polysemy-chronos = jailbreak;
      polysemy-conc = jailbreak;
      polysemy-http = jailbreak;
      polysemy-log = jailbreak;
      polysemy-process = jailbreak;
      polysemy-resume = jailbreak;
      polysemy-test = jailbreak;
      polysemy-time = jailbreak;
      prelate = jailbreak;
      streamly = force;
      unicode-data = notest;
      zeugma = jailbreak;
    };

    buildInputs = pkgs: [pkgs.neovim pkgs.tmux pkgs.xterm];

    packages.ribosome-host = {
      src = ./packages/host;

      cabal.meta.synopsis = "Neovim plugin host for Polysemy";

      library = {
        enable = true;
        dependencies = [
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
          "text"
          "typed-process"
        ];
        reexported-modules = ["Data.MessagePack"];
      };

      test = {
        enable = true;
        dependencies = [
          "chronos"
          "deepseq"
          "exon"
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

      override = api: api.configure "--extra-prog-path=${api.pkgs.neovim}/bin";

    };

    packages.ribosome-host-test = {
      src = ./packages/host-test;

      cabal.meta.synopsis = "Test tools for ribosome-host";

      library = {
        enable = true;
        dependencies = [
          "chronos"
          "hedgehog"
          "ribosome-host"
          "polysemy-chronos"
          "polysemy-test"
        ];
      };

    };

    packages.ribosome = {
      src = ./packages/ribosome;

      cabal.meta.synopsis = "Neovim plugin framework for Polysemy";

      library = {
        enable = true;
        dependencies = [
          "aeson"
          "exon"
          "extra"
          "messagepack"
          "optparse-applicative"
          "path"
          "path-io"
          "prettyprinter"
          "ribosome-host"
        ];
        reexported-modules = ["Data.MessagePack"];
      };

      test = {
        enable = true;
        dependencies = [
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

    };

    packages.ribosome-app = {
      src = ./packages/app;

      cabal.meta.synopsis = "CLI for Ribosome";

      library = {
        enable = true;
        dependencies = [
          "exon"
          "optparse-applicative"
          "path"
          "path-io"
          "polysemy-chronos"
          "rainbow"
          "ribosome-host"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "chronos"
          "path"
          "polysemy-test"
          "tasty"
        ];
      };

      executables.ribosome.source-dirs = "app";

    };

    packages.ribosome-test = {
      src = ./packages/test;

      cabal.meta.synopsis = "Test tools for Ribosome";

      library = {
        enable = true;
        dependencies = [
          "chiasma"
          "chiasma-test"
          "exon"
          "hedgehog"
          "lens-regex-pcre"
          "path"
          "path-io"
          "polysemy-chronos"
          "polysemy-test"
          "ribosome"
          "ribosome-host"
          "ribosome-host-test"
          "tasty"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "exon"
          "polysemy-test"
          "ribosome"
          "ribosome-host"
          "tasty"
        ];
      };

    };

    packages.ribosome-menu = {
      src = ./packages/menu;

      cabal.meta.synopsis = "Menu widget for Ribosome";

      library = {
        enable = true;
        dependencies = [
          "bytestring-trie"
          "exon"
          "extra"
          "fuzzyfind"
          "lens-regex-pcre"
          "microlens-mtl"
          "pcre-light"
          "polysemy-chronos"
          "polysemy-conc"
          "ribosome"
          "ribosome-host"
          "streamly"
          "streamly-core"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "exon"
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
          "zeugma"
        ];
      };

      benchmark = {
        enable = true;
        dependencies = [
          "criterion"
          "exon"
          "microlens"
          "ribosome"
          "ribosome-host"
          "path"
          "polysemy-conc"
          "polysemy-test"
          "streamly"
        ];
      };

    };

    packages.integration = {
      src = ./packages/integration;

      cabal.meta.synopsis = "Internal project for testing Ribosome";

      library = {
        enable = true;
        dependencies = [
        "messagepack"
        "ribosome"
        "ribosome-host"
        ];
      };

      test = {
        enable = true;
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

    envs.dev = {
      env = { RIBOSOME_ROOT = builtins.toPath self; };
    };

    outputs.apps = import ./ops/template-apps.nix { inherit self config lib; };

  }) // {

    lib = hix.lib.extend (_: super: {
      flakeWith = {projectModules ? [], extraModules ? []}:
      super.flakeWith { inherit projectModules; extraModules = extraModules ++ [(import ./ops/api.nix self)]; };
    });
  };

}
