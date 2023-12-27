{
  description = "A Neovim plugin host and framework for Haskell";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
  };

  outputs = {self, hix, ...}: hix.lib.pro ({config, lib, ...}: {
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
          version = ">= 0.6 && < 0.8";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy ^>= 1.9" "polysemy-plugin ^>= 0.4"];
    };

    overrides = {hackage, pkgs, jailbreak, ...}: {
      fuzzyfind = jailbreak (hackage "3.0.1" "17lk2i3gq5kg7h2a4cax6n4lz2mh0qqyrw34lccnwr7nlvpg4var");
      streamly = jailbreak (hackage "0.8.3" "1w8nhss2rcpbphw2b0d86z7jqxpx38gfn4ahdmd7afg65gzx5bq3");
      unicode-data = hackage "0.3.1" "0pw8cq3spzqdbpyvg026qqyfdva40php7gd728cc56csawpahyx7";
      # Inherited, repeated here to reduce evaluation size
      chiasma = hackage "0.10.1.0" "1ch2rys4whmldv8y05mipv047lgjv07rmf2sihpl94kc24pc37qs";
      chiasma-test = hackage "0.10.1.0" "1r9svmca39pi05s1vsa1fs9hgp93ivsi9qzjsdhmfk2k86dc30hh";
      exon = hackage "1.5.0.0" "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
      flatparse = hackage "0.5.0.1" "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
      incipit = hackage "0.9.0.1" "13qp45wry6xs54fhkcvydnz9b3nqd88sg1ypg5kpl9af4z9gqd3s";
      polysemy = hackage "1.9.1.2" "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
      polysemy-chronos = hackage "0.6.0.2" "1wvjpl2axxhywjj7z1hjg16sxldq0x63md4rzf1mvdn8067mg35s";
      polysemy-conc = hackage "0.13.0.1" "01zfjx1kmrw5hnqyckrwwkdzjbihfn6y516lw7lffhqfp354522b";
      polysemy-http = jailbreak (hackage "0.13.0.1" "0zg9dhkbsy3sn7gs0axrn4y9z12jqn1138lbcz4lis0s8fjh0zj2");
      polysemy-log = hackage "0.10.0.1" "1vwlj7xpr4v4340mx8ylfrn2wikix0lkbhg86bikpkzhhk1w3q7q";
      polysemy-plugin = hackage "0.4.5.1" "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
      polysemy-process = hackage "0.13.0.1" "0jzcr0vvmnmpvyyk062lq1k4xcyph9zn6b80wwn6h484qjpwpqcd";
      polysemy-resume = hackage "0.8.0.1" "1fci0v1xc6xx8qkj8s57m7yy2w1rxyxvb9bw9vkksdxr3z38dbkg";
      polysemy-test = hackage "0.9.0.0" "1adkp48v04klsjyv8846w7ryf1fiqxb4ga69mps9vg2bp9fj5i7j";
      polysemy-time = hackage "0.6.0.2" "198x2wimdzk93hz0bq2k7wjibcjvzm38m6fica1jfcbh4p531swp";
      prelate = jailbreak (hackage "0.7.0.1" "0qy0dkckvlbinp1gm85ziiyml0lj57b93qnz23ldjmbj4skcp8s8");
      zeugma = hackage "0.9.0.1" "1clsd2c26cp60kajf4aw8wydnmvgr4blka8yzysi3gzd8ky32ck1";
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
