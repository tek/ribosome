{
  dev = {
    chiasma = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.12.1.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    chiasma-test = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.12.3.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-process polysemy-test prelate tasty
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-conc = {
  meta = {
    sha256 = "00ds083rpahv3q5n355hcbgv1ba7l121bpj642pkc7z0lpciq0z5";
    ver = "0.15.0.0";
  };
  drv = { mkDerivation, async, base, hedgehog, incipit-core, lib, polysemy
, polysemy-plugin, polysemy-resume, polysemy-test, polysemy-time
, stm, stm-chans, tasty, tasty-hedgehog, time, torsor, unagi-chan
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.15.0.0";
  src = /nix/store/gjpqg6nqs2kxqgawxckyj52lpwshqi5v-source;
  libraryHaskellDepends = [
    async base incipit-core polysemy polysemy-resume polysemy-time stm
    stm-chans torsor unagi-chan
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time tasty tasty-hedgehog time torsor
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-test = {
  meta = {
    sha256 = "0faajcwslgkjigakimz5sxvcd92p8vdzafway8js8622jmprjqjb";
    ver = "0.11.0.1";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.11.0.1";
  src = /nix/store/9wmv0p0kdcd3ccqa13wg4h4d1jfis948-source;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base hedgehog incipit-core path path-io polysemy tasty
    tasty-hedgehog transformers
  ];
  testHaskellDepends = [
    base hedgehog incipit-core path polysemy tasty
  ];
  homepage = "https://github.com/tek/polysemy-test#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    prelate = {
  meta = {
    sha256 = "0pbfaxl06gxkzqbj6f96jf3xv0g59cwawvw4xsw2h8bnszq43zyz";
    ver = "0.9.0.2";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.9.0.2";
  src = /nix/store/q4a8a9pvvh69nc9jx3bqmqdl6hlv6ygk-source;
  libraryHaskellDepends = [
    aeson base exon extra generic-lens incipit microlens microlens-ghc
    polysemy-chronos polysemy-conc polysemy-log polysemy-process
    polysemy-resume polysemy-time template-haskell
  ];
  homepage = "https://github.com/tek/prelate#readme";
  description = "A Prelude";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    zeugma = {
  meta = {
    sha256 = "1f8rphznwrdsy4p08xy4cbgql5i87fx1l4b7s06nq92j58rvfw7n";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-process, polysemy-test, tasty
, tasty-expected-failure, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.11.0.0";
  src = /nix/store/pxibdqn13vwhzl7hgkgdpwp6h8ylfcva-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-process polysemy-test tasty tasty-expected-failure
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  };
  min = {
    chiasma = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.12.1.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    chiasma-test = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.12.3.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-process polysemy-test prelate tasty
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-conc = {
  meta = {
    sha256 = "00ds083rpahv3q5n355hcbgv1ba7l121bpj642pkc7z0lpciq0z5";
    ver = "0.15.0.0";
  };
  drv = { mkDerivation, async, base, hedgehog, incipit-core, lib, polysemy
, polysemy-plugin, polysemy-resume, polysemy-test, polysemy-time
, stm, stm-chans, tasty, tasty-hedgehog, time, torsor, unagi-chan
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.15.0.0";
  src = /nix/store/gjpqg6nqs2kxqgawxckyj52lpwshqi5v-source;
  libraryHaskellDepends = [
    async base incipit-core polysemy polysemy-resume polysemy-time stm
    stm-chans torsor unagi-chan
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time tasty tasty-hedgehog time torsor
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-test = {
  meta = {
    sha256 = "0faajcwslgkjigakimz5sxvcd92p8vdzafway8js8622jmprjqjb";
    ver = "0.11.0.1";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.11.0.1";
  src = /nix/store/9wmv0p0kdcd3ccqa13wg4h4d1jfis948-source;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base hedgehog incipit-core path path-io polysemy tasty
    tasty-hedgehog transformers
  ];
  testHaskellDepends = [
    base hedgehog incipit-core path polysemy tasty
  ];
  homepage = "https://github.com/tek/polysemy-test#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    prelate = {
  meta = {
    sha256 = "0pbfaxl06gxkzqbj6f96jf3xv0g59cwawvw4xsw2h8bnszq43zyz";
    ver = "0.9.0.2";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.9.0.2";
  src = /nix/store/q4a8a9pvvh69nc9jx3bqmqdl6hlv6ygk-source;
  libraryHaskellDepends = [
    aeson base exon extra generic-lens incipit microlens microlens-ghc
    polysemy-chronos polysemy-conc polysemy-log polysemy-process
    polysemy-resume polysemy-time template-haskell
  ];
  homepage = "https://github.com/tek/prelate#readme";
  description = "A Prelude";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    zeugma = {
  meta = {
    sha256 = "1f8rphznwrdsy4p08xy4cbgql5i87fx1l4b7s06nq92j58rvfw7n";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-process, polysemy-test, tasty
, tasty-expected-failure, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.11.0.0";
  src = /nix/store/pxibdqn13vwhzl7hgkgdpwp6h8ylfcva-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-process polysemy-test tasty tasty-expected-failure
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  };
  profiled = {
    chiasma = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.12.1.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/chiasma;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    chiasma-test = {
  meta = {
    src = "/nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.12.3.0";
  src = /nix/store/nzw4c8lmasnrjjk954a13dr1qcx5b382-source/packages/test;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-process polysemy-test prelate tasty
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-conc = {
  meta = {
    sha256 = "00ds083rpahv3q5n355hcbgv1ba7l121bpj642pkc7z0lpciq0z5";
    ver = "0.15.0.0";
  };
  drv = { mkDerivation, async, base, hedgehog, incipit-core, lib, polysemy
, polysemy-plugin, polysemy-resume, polysemy-test, polysemy-time
, stm, stm-chans, tasty, tasty-hedgehog, time, torsor, unagi-chan
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.15.0.0";
  src = /nix/store/gjpqg6nqs2kxqgawxckyj52lpwshqi5v-source;
  libraryHaskellDepends = [
    async base incipit-core polysemy polysemy-resume polysemy-time stm
    stm-chans torsor unagi-chan
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time tasty tasty-hedgehog time torsor
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    polysemy-test = {
  meta = {
    sha256 = "0faajcwslgkjigakimz5sxvcd92p8vdzafway8js8622jmprjqjb";
    ver = "0.11.0.1";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.11.0.1";
  src = /nix/store/9wmv0p0kdcd3ccqa13wg4h4d1jfis948-source;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base hedgehog incipit-core path path-io polysemy tasty
    tasty-hedgehog transformers
  ];
  testHaskellDepends = [
    base hedgehog incipit-core path polysemy tasty
  ];
  homepage = "https://github.com/tek/polysemy-test#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    prelate = {
  meta = {
    sha256 = "0pbfaxl06gxkzqbj6f96jf3xv0g59cwawvw4xsw2h8bnszq43zyz";
    ver = "0.9.0.2";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.9.0.2";
  src = /nix/store/q4a8a9pvvh69nc9jx3bqmqdl6hlv6ygk-source;
  libraryHaskellDepends = [
    aeson base exon extra generic-lens incipit microlens microlens-ghc
    polysemy-chronos polysemy-conc polysemy-log polysemy-process
    polysemy-resume polysemy-time template-haskell
  ];
  homepage = "https://github.com/tek/prelate#readme";
  description = "A Prelude";
  license = "BSD-2-Clause-Patent";
}
;
}
;
    zeugma = {
  meta = {
    sha256 = "1f8rphznwrdsy4p08xy4cbgql5i87fx1l4b7s06nq92j58rvfw7n";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-process, polysemy-test, tasty
, tasty-expected-failure, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.11.0.0";
  src = /nix/store/pxibdqn13vwhzl7hgkgdpwp6h8ylfcva-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-process polysemy-test tasty tasty-expected-failure
    tasty-hedgehog
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  };
}