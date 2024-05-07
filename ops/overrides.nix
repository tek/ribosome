{
dev = {
  chiasma = {
  meta = {
    sha256 = "1ch2rys4whmldv8y05mipv047lgjv07rmf2sihpl94kc24pc37qs";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.10.1.0";
  src = /nix/store/rs76nw38dsan9f2k1z8d1wribnhj8nar-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
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
    sha256 = "1r9svmca39pi05s1vsa1fs9hgp93ivsi9qzjsdhmfk2k86dc30hh";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.10.1.0";
  src = /nix/store/gwd60fzijn8yn5c2mjxla3qdj5p0yi3g-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
  fuzzyfind = {
  meta = {
    sha256 = "17lk2i3gq5kg7h2a4cax6n4lz2mh0qqyrw34lccnwr7nlvpg4var";
    ver = "3.0.1";
  };
  drv = { mkDerivation, base, containers, criterion, deepseq, hspec, lib
, massiv, QuickCheck, text
}:
mkDerivation {
  pname = "fuzzyfind";
  version = "3.0.1";
  src = /nix/store/76z1vv2s7jwnwzf9i949z6qh6if7pbgp-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers massiv text ];
  executableHaskellDepends = [ base criterion deepseq ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  homepage = "http://github.com/runarorama/fuzzyfind/";
  description = "Fuzzy text matching";
  license = lib.licenses.mit;
  mainProgram = "bench";
}
;
}
;
  incipit = {
  meta = {
    sha256 = "13qp45wry6xs54fhkcvydnz9b3nqd88sg1ypg5kpl9af4z9gqd3s";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy-conc
, polysemy-log, polysemy-resume, polysemy-time
}:
mkDerivation {
  pname = "incipit";
  version = "0.9.0.1";
  src = /nix/store/hgi93s4kpm42yha6yba721w2x8szyd6i-source;
  libraryHaskellDepends = [
    base incipit-core polysemy-conc polysemy-log polysemy-resume
    polysemy-time
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "A Prelude for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy = {
  meta = {
    sha256 = "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
    ver = "1.9.1.2";
  };
  drv = { mkDerivation, async, base, Cabal, cabal-doctest, containers
, doctest, first-class-families, hspec, hspec-discover
, inspection-testing, lib, mtl, stm, syb, template-haskell
, th-abstraction, transformers, type-errors, unagi-chan
}:
mkDerivation {
  pname = "polysemy";
  version = "1.9.1.2";
  src = /nix/store/anfwczj25hh5zcm9y70vb1221wayi1v0-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    async base containers first-class-families mtl stm syb
    template-haskell th-abstraction transformers type-errors unagi-chan
  ];
  testHaskellDepends = [
    async base containers doctest first-class-families hspec
    hspec-discover inspection-testing mtl stm syb template-haskell
    th-abstraction transformers type-errors unagi-chan
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Higher-order, low-boilerplate free monads";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-chronos = {
  meta = {
    sha256 = "1wvjpl2axxhywjj7z1hjg16sxldq0x63md4rzf1mvdn8067mg35s";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, base, chronos, incipit-core, lib, polysemy-test
, polysemy-time, tasty
}:
mkDerivation {
  pname = "polysemy-chronos";
  version = "0.6.0.2";
  src = /nix/store/4ggkm9nwqlxwix3869ydvhf8dzw0gl4v-source;
  libraryHaskellDepends = [
    base chronos incipit-core polysemy-time
  ];
  testHaskellDepends = [
    base chronos incipit-core polysemy-test polysemy-time tasty
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for Chronos";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-conc = {
  meta = {
    sha256 = "01zfjx1kmrw5hnqyckrwwkdzjbihfn6y516lw7lffhqfp354522b";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, async, base, containers, hedgehog, incipit-core
, lib, polysemy, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, stm, stm-chans, tasty, tasty-hedgehog, time
, torsor, unagi-chan, unix
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.13.0.1";
  src = /nix/store/paa9q65s648gd4hxlfipxp5ya96cp237-source;
  libraryHaskellDepends = [
    async base containers incipit-core polysemy polysemy-resume
    polysemy-time stm stm-chans torsor unagi-chan unix
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time stm tasty tasty-hedgehog time unix
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-http = {
  meta = {
    sha256 = "0zg9dhkbsy3sn7gs0axrn4y9z12jqn1138lbcz4lis0s8fjh0zj2";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, aeson, base, case-insensitive, exon, hedgehog
, http-client, http-client-tls, http-types, lib, network, polysemy
, polysemy-plugin, prelate, servant-client, servant-server, tasty
, tasty-hedgehog, time, warp
}:
mkDerivation {
  pname = "polysemy-http";
  version = "0.13.0.1";
  src = /nix/store/1jnlr3wdfc8rzx7a8sbb42bzbcgm39i3-source;
  libraryHaskellDepends = [
    aeson base case-insensitive exon http-client http-client-tls
    http-types polysemy polysemy-plugin prelate time
  ];
  testHaskellDepends = [
    aeson base exon hedgehog http-client network polysemy
    polysemy-plugin prelate servant-client servant-server tasty
    tasty-hedgehog warp
  ];
  homepage = "https://github.com/tek/polysemy-http#readme";
  description = "Polysemy effects for HTTP clients";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-log = {
  meta = {
    sha256 = "1vwlj7xpr4v4340mx8ylfrn2wikix0lkbhg86bikpkzhhk1w3q7q";
    ver = "0.10.0.1";
  };
  drv = { mkDerivation, ansi-terminal, async, base, incipit-core, lib
, polysemy, polysemy-conc, polysemy-plugin, polysemy-test
, polysemy-time, stm, tasty, time
}:
mkDerivation {
  pname = "polysemy-log";
  version = "0.10.0.1";
  src = /nix/store/rjmszzg4h13q7q8sxz1y4z32ay9xqxmn-source;
  libraryHaskellDepends = [
    ansi-terminal async base incipit-core polysemy polysemy-conc
    polysemy-time stm time
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-test polysemy-time tasty time
  ];
  benchmarkHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
  ];
  homepage = "https://github.com/tek/polysemy-log#readme";
  description = "Polysemy effects for logging";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-plugin = {
  meta = {
    sha256 = "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
    ver = "0.4.5.1";
  };
  drv = { mkDerivation, base, Cabal, cabal-doctest, containers, doctest
, ghc, ghc-tcplugins-extra, hspec, hspec-discover
, inspection-testing, lib, polysemy, should-not-typecheck, syb
, transformers
}:
mkDerivation {
  pname = "polysemy-plugin";
  version = "0.4.5.1";
  src = /nix/store/02adx7h7zmis7gay1h0irskrkp7hbql5-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base containers ghc ghc-tcplugins-extra polysemy syb transformers
  ];
  testHaskellDepends = [
    base containers doctest ghc ghc-tcplugins-extra hspec
    hspec-discover inspection-testing polysemy should-not-typecheck syb
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Disambiguate obvious uses of effects";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-process = {
  meta = {
    sha256 = "0jzcr0vvmnmpvyyk062lq1k4xcyph9zn6b80wwn6h484qjpwpqcd";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, path, path-io, polysemy
, polysemy-conc, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, posix-pty, process, stm-chans, tasty
, tasty-expected-failure, typed-process, unix
}:
mkDerivation {
  pname = "polysemy-process";
  version = "0.13.0.1";
  src = /nix/store/6dyq47jsbbh5gh58kjapf35bqll05pk4-source;
  libraryHaskellDepends = [
    base incipit-core path path-io polysemy polysemy-conc
    polysemy-resume polysemy-time posix-pty process stm-chans
    typed-process unix
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-resume polysemy-test polysemy-time tasty
    tasty-expected-failure typed-process
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for system processes";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-resume = {
  meta = {
    sha256 = "1fci0v1xc6xx8qkj8s57m7yy2w1rxyxvb9bw9vkksdxr3z38dbkg";
    ver = "0.8.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy, polysemy-plugin
, polysemy-test, stm, tasty, transformers
}:
mkDerivation {
  pname = "polysemy-resume";
  version = "0.8.0.1";
  src = /nix/store/h2ayy5qlbfl8qlxa0ivyxq6f0dyhar0v-source;
  libraryHaskellDepends = [
    base incipit-core polysemy transformers
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-plugin polysemy-test stm tasty
  ];
  homepage = "https://github.com/tek/polysemy-resume#readme";
  description = "Polysemy error tracking";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-test = {
  meta = {
    sha256 = "1adkp48v04klsjyv8846w7ryf1fiqxb4ga69mps9vg2bp9fj5i7j";
    ver = "0.9.0.0";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.9.0.0";
  src = /nix/store/jjk10ix28006wr8ljfv4pj6k76xckpsn-source;
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
  polysemy-time = {
  meta = {
    sha256 = "198x2wimdzk93hz0bq2k7wjibcjvzm38m6fica1jfcbh4p531swp";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, aeson, base, incipit-core, lib, polysemy-test
, tasty, template-haskell, time, torsor
}:
mkDerivation {
  pname = "polysemy-time";
  version = "0.6.0.2";
  src = /nix/store/pflvfr8qg2snw2shhvpwrlz1f51jfyyg-source;
  libraryHaskellDepends = [
    aeson base incipit-core template-haskell time torsor
  ];
  testHaskellDepends = [
    base incipit-core polysemy-test tasty time
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for time";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  prelate = {
  meta = {
    sha256 = "0qy0dkckvlbinp1gm85ziiyml0lj57b93qnz23ldjmbj4skcp8s8";
    ver = "0.7.0.1";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.7.0.1";
  src = /nix/store/b8rilflbwslwc75xamzvkhn5xxnvn6xw-source;
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
  streamly = {
  meta = {
    sha256 = "1w8nhss2rcpbphw2b0d86z7jqxpx38gfn4ahdmd7afg65gzx5bq3";
    ver = "0.8.3";
  };
  drv = { mkDerivation, atomic-primops, base, containers, deepseq
, directory, exceptions, filepath, fusion-plugin-types, ghc-prim
, heaps, lib, lockfree-queue, monad-control, mtl, network
, primitive, transformers, transformers-base, unicode-data
}:
mkDerivation {
  pname = "streamly";
  version = "0.8.3";
  src = /nix/store/vgpkpqqmdrq6860xiw3nvh0nm81ji54p-source;
  libraryHaskellDepends = [
    atomic-primops base containers deepseq directory exceptions
    filepath fusion-plugin-types ghc-prim heaps lockfree-queue
    monad-control mtl network primitive transformers transformers-base
    unicode-data
  ];
  homepage = "https://streamly.composewell.com";
  description = "Dataflow programming and declarative concurrency";
  license = lib.licenses.bsd3;
}
;
}
;
  unicode-data = {
  meta = {
    sha256 = "0pw8cq3spzqdbpyvg026qqyfdva40php7gd728cc56csawpahyx7";
    ver = "0.3.1";
  };
  drv = { mkDerivation, base, deepseq, hspec, hspec-discover, lib, tasty
, tasty-bench
}:
mkDerivation {
  pname = "unicode-data";
  version = "0.3.1";
  src = /nix/store/dgk7n201b9diibh12ajmb12g7dvqs2xm-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base deepseq tasty tasty-bench ];
  homepage = "http://github.com/composewell/unicode-data";
  description = "Access Unicode Character Database (UCD)";
  license = lib.licenses.asl20;
}
;
}
;
  zeugma = {
  meta = {
    sha256 = "1clsd2c26cp60kajf4aw8wydnmvgr4blka8yzysi3gzd8ky32ck1";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-test, tasty, tasty-expected-failure
, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.9.0.1";
  src = /nix/store/l3sj8fw2rjv42a4bgjqbndpmp3f3yadw-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-test tasty tasty-expected-failure tasty-hedgehog
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "Polysemy effects for testing";
  license = "BSD-2-Clause-Patent";
}
;
}
;
};
hls = {
};
min = {
  chiasma = {
  meta = {
    sha256 = "1ch2rys4whmldv8y05mipv047lgjv07rmf2sihpl94kc24pc37qs";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.10.1.0";
  src = /nix/store/rs76nw38dsan9f2k1z8d1wribnhj8nar-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
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
    sha256 = "1r9svmca39pi05s1vsa1fs9hgp93ivsi9qzjsdhmfk2k86dc30hh";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.10.1.0";
  src = /nix/store/gwd60fzijn8yn5c2mjxla3qdj5p0yi3g-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
  fuzzyfind = {
  meta = {
    sha256 = "17lk2i3gq5kg7h2a4cax6n4lz2mh0qqyrw34lccnwr7nlvpg4var";
    ver = "3.0.1";
  };
  drv = { mkDerivation, base, containers, criterion, deepseq, hspec, lib
, massiv, QuickCheck, text
}:
mkDerivation {
  pname = "fuzzyfind";
  version = "3.0.1";
  src = /nix/store/76z1vv2s7jwnwzf9i949z6qh6if7pbgp-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers massiv text ];
  executableHaskellDepends = [ base criterion deepseq ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  homepage = "http://github.com/runarorama/fuzzyfind/";
  description = "Fuzzy text matching";
  license = lib.licenses.mit;
  mainProgram = "bench";
}
;
}
;
  incipit = {
  meta = {
    sha256 = "13qp45wry6xs54fhkcvydnz9b3nqd88sg1ypg5kpl9af4z9gqd3s";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy-conc
, polysemy-log, polysemy-resume, polysemy-time
}:
mkDerivation {
  pname = "incipit";
  version = "0.9.0.1";
  src = /nix/store/hgi93s4kpm42yha6yba721w2x8szyd6i-source;
  libraryHaskellDepends = [
    base incipit-core polysemy-conc polysemy-log polysemy-resume
    polysemy-time
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "A Prelude for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy = {
  meta = {
    sha256 = "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
    ver = "1.9.1.2";
  };
  drv = { mkDerivation, async, base, Cabal, cabal-doctest, containers
, doctest, first-class-families, hspec, hspec-discover
, inspection-testing, lib, mtl, stm, syb, template-haskell
, th-abstraction, transformers, type-errors, unagi-chan
}:
mkDerivation {
  pname = "polysemy";
  version = "1.9.1.2";
  src = /nix/store/anfwczj25hh5zcm9y70vb1221wayi1v0-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    async base containers first-class-families mtl stm syb
    template-haskell th-abstraction transformers type-errors unagi-chan
  ];
  testHaskellDepends = [
    async base containers doctest first-class-families hspec
    hspec-discover inspection-testing mtl stm syb template-haskell
    th-abstraction transformers type-errors unagi-chan
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Higher-order, low-boilerplate free monads";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-chronos = {
  meta = {
    sha256 = "1wvjpl2axxhywjj7z1hjg16sxldq0x63md4rzf1mvdn8067mg35s";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, base, chronos, incipit-core, lib, polysemy-test
, polysemy-time, tasty
}:
mkDerivation {
  pname = "polysemy-chronos";
  version = "0.6.0.2";
  src = /nix/store/4ggkm9nwqlxwix3869ydvhf8dzw0gl4v-source;
  libraryHaskellDepends = [
    base chronos incipit-core polysemy-time
  ];
  testHaskellDepends = [
    base chronos incipit-core polysemy-test polysemy-time tasty
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for Chronos";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-conc = {
  meta = {
    sha256 = "01zfjx1kmrw5hnqyckrwwkdzjbihfn6y516lw7lffhqfp354522b";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, async, base, containers, hedgehog, incipit-core
, lib, polysemy, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, stm, stm-chans, tasty, tasty-hedgehog, time
, torsor, unagi-chan, unix
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.13.0.1";
  src = /nix/store/paa9q65s648gd4hxlfipxp5ya96cp237-source;
  libraryHaskellDepends = [
    async base containers incipit-core polysemy polysemy-resume
    polysemy-time stm stm-chans torsor unagi-chan unix
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time stm tasty tasty-hedgehog time unix
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-http = {
  meta = {
    sha256 = "0zg9dhkbsy3sn7gs0axrn4y9z12jqn1138lbcz4lis0s8fjh0zj2";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, aeson, base, case-insensitive, exon, hedgehog
, http-client, http-client-tls, http-types, lib, network, polysemy
, polysemy-plugin, prelate, servant-client, servant-server, tasty
, tasty-hedgehog, time, warp
}:
mkDerivation {
  pname = "polysemy-http";
  version = "0.13.0.1";
  src = /nix/store/1jnlr3wdfc8rzx7a8sbb42bzbcgm39i3-source;
  libraryHaskellDepends = [
    aeson base case-insensitive exon http-client http-client-tls
    http-types polysemy polysemy-plugin prelate time
  ];
  testHaskellDepends = [
    aeson base exon hedgehog http-client network polysemy
    polysemy-plugin prelate servant-client servant-server tasty
    tasty-hedgehog warp
  ];
  homepage = "https://github.com/tek/polysemy-http#readme";
  description = "Polysemy effects for HTTP clients";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-log = {
  meta = {
    sha256 = "1vwlj7xpr4v4340mx8ylfrn2wikix0lkbhg86bikpkzhhk1w3q7q";
    ver = "0.10.0.1";
  };
  drv = { mkDerivation, ansi-terminal, async, base, incipit-core, lib
, polysemy, polysemy-conc, polysemy-plugin, polysemy-test
, polysemy-time, stm, tasty, time
}:
mkDerivation {
  pname = "polysemy-log";
  version = "0.10.0.1";
  src = /nix/store/rjmszzg4h13q7q8sxz1y4z32ay9xqxmn-source;
  libraryHaskellDepends = [
    ansi-terminal async base incipit-core polysemy polysemy-conc
    polysemy-time stm time
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-test polysemy-time tasty time
  ];
  benchmarkHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
  ];
  homepage = "https://github.com/tek/polysemy-log#readme";
  description = "Polysemy effects for logging";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-plugin = {
  meta = {
    sha256 = "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
    ver = "0.4.5.1";
  };
  drv = { mkDerivation, base, Cabal, cabal-doctest, containers, doctest
, ghc, ghc-tcplugins-extra, hspec, hspec-discover
, inspection-testing, lib, polysemy, should-not-typecheck, syb
, transformers
}:
mkDerivation {
  pname = "polysemy-plugin";
  version = "0.4.5.1";
  src = /nix/store/02adx7h7zmis7gay1h0irskrkp7hbql5-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base containers ghc ghc-tcplugins-extra polysemy syb transformers
  ];
  testHaskellDepends = [
    base containers doctest ghc ghc-tcplugins-extra hspec
    hspec-discover inspection-testing polysemy should-not-typecheck syb
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Disambiguate obvious uses of effects";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-process = {
  meta = {
    sha256 = "0jzcr0vvmnmpvyyk062lq1k4xcyph9zn6b80wwn6h484qjpwpqcd";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, path, path-io, polysemy
, polysemy-conc, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, posix-pty, process, stm-chans, tasty
, tasty-expected-failure, typed-process, unix
}:
mkDerivation {
  pname = "polysemy-process";
  version = "0.13.0.1";
  src = /nix/store/6dyq47jsbbh5gh58kjapf35bqll05pk4-source;
  libraryHaskellDepends = [
    base incipit-core path path-io polysemy polysemy-conc
    polysemy-resume polysemy-time posix-pty process stm-chans
    typed-process unix
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-resume polysemy-test polysemy-time tasty
    tasty-expected-failure typed-process
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for system processes";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-resume = {
  meta = {
    sha256 = "1fci0v1xc6xx8qkj8s57m7yy2w1rxyxvb9bw9vkksdxr3z38dbkg";
    ver = "0.8.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy, polysemy-plugin
, polysemy-test, stm, tasty, transformers
}:
mkDerivation {
  pname = "polysemy-resume";
  version = "0.8.0.1";
  src = /nix/store/h2ayy5qlbfl8qlxa0ivyxq6f0dyhar0v-source;
  libraryHaskellDepends = [
    base incipit-core polysemy transformers
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-plugin polysemy-test stm tasty
  ];
  homepage = "https://github.com/tek/polysemy-resume#readme";
  description = "Polysemy error tracking";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-test = {
  meta = {
    sha256 = "1adkp48v04klsjyv8846w7ryf1fiqxb4ga69mps9vg2bp9fj5i7j";
    ver = "0.9.0.0";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.9.0.0";
  src = /nix/store/jjk10ix28006wr8ljfv4pj6k76xckpsn-source;
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
  polysemy-time = {
  meta = {
    sha256 = "198x2wimdzk93hz0bq2k7wjibcjvzm38m6fica1jfcbh4p531swp";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, aeson, base, incipit-core, lib, polysemy-test
, tasty, template-haskell, time, torsor
}:
mkDerivation {
  pname = "polysemy-time";
  version = "0.6.0.2";
  src = /nix/store/pflvfr8qg2snw2shhvpwrlz1f51jfyyg-source;
  libraryHaskellDepends = [
    aeson base incipit-core template-haskell time torsor
  ];
  testHaskellDepends = [
    base incipit-core polysemy-test tasty time
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for time";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  prelate = {
  meta = {
    sha256 = "0qy0dkckvlbinp1gm85ziiyml0lj57b93qnz23ldjmbj4skcp8s8";
    ver = "0.7.0.1";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.7.0.1";
  src = /nix/store/b8rilflbwslwc75xamzvkhn5xxnvn6xw-source;
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
  streamly = {
  meta = {
    sha256 = "1w8nhss2rcpbphw2b0d86z7jqxpx38gfn4ahdmd7afg65gzx5bq3";
    ver = "0.8.3";
  };
  drv = { mkDerivation, atomic-primops, base, containers, deepseq
, directory, exceptions, filepath, fusion-plugin-types, ghc-prim
, heaps, lib, lockfree-queue, monad-control, mtl, network
, primitive, transformers, transformers-base, unicode-data
}:
mkDerivation {
  pname = "streamly";
  version = "0.8.3";
  src = /nix/store/vgpkpqqmdrq6860xiw3nvh0nm81ji54p-source;
  libraryHaskellDepends = [
    atomic-primops base containers deepseq directory exceptions
    filepath fusion-plugin-types ghc-prim heaps lockfree-queue
    monad-control mtl network primitive transformers transformers-base
    unicode-data
  ];
  homepage = "https://streamly.composewell.com";
  description = "Dataflow programming and declarative concurrency";
  license = lib.licenses.bsd3;
}
;
}
;
  unicode-data = {
  meta = {
    sha256 = "0pw8cq3spzqdbpyvg026qqyfdva40php7gd728cc56csawpahyx7";
    ver = "0.3.1";
  };
  drv = { mkDerivation, base, deepseq, hspec, hspec-discover, lib, tasty
, tasty-bench
}:
mkDerivation {
  pname = "unicode-data";
  version = "0.3.1";
  src = /nix/store/dgk7n201b9diibh12ajmb12g7dvqs2xm-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base deepseq tasty tasty-bench ];
  homepage = "http://github.com/composewell/unicode-data";
  description = "Access Unicode Character Database (UCD)";
  license = lib.licenses.asl20;
}
;
}
;
  zeugma = {
  meta = {
    sha256 = "1clsd2c26cp60kajf4aw8wydnmvgr4blka8yzysi3gzd8ky32ck1";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-test, tasty, tasty-expected-failure
, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.9.0.1";
  src = /nix/store/l3sj8fw2rjv42a4bgjqbndpmp3f3yadw-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-test tasty tasty-expected-failure tasty-hedgehog
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
    sha256 = "1ch2rys4whmldv8y05mipv047lgjv07rmf2sihpl94kc24pc37qs";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.10.1.0";
  src = /nix/store/rs76nw38dsan9f2k1z8d1wribnhj8nar-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
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
    sha256 = "1r9svmca39pi05s1vsa1fs9hgp93ivsi9qzjsdhmfk2k86dc30hh";
    ver = "0.10.1.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.10.1.0";
  src = /nix/store/gwd60fzijn8yn5c2mjxla3qdj5p0yi3g-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  exon = {
  meta = {
    sha256 = "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
    ver = "1.5.0.0";
  };
  drv = { mkDerivation, base, criterion, flatparse, generics-sop
, ghc-hs-meta, hedgehog, incipit-base, lib, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "exon";
  version = "1.5.0.0";
  src = /nix/store/jh1njmdacy2200l6ixaiirgr8lbnwj7v-source;
  libraryHaskellDepends = [
    base flatparse generics-sop ghc-hs-meta incipit-base
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog incipit-base tasty tasty-hedgehog template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion incipit-base ];
  homepage = "https://github.com/tek/exon#readme";
  description = "Customizable quasiquote interpolation";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  flatparse = {
  meta = {
    sha256 = "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    ver = "0.5.0.1";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, containers, gauge
, hspec, HUnit, integer-gmp, lib, megaparsec, parsec, primitive
, QuickCheck, quickcheck-instances, template-haskell, utf8-string
}:
mkDerivation {
  pname = "flatparse";
  version = "0.5.0.1";
  src = /nix/store/xkg9z7kvk4a3v2dfl0mh3sz50wl56srx-source;
  libraryHaskellDepends = [
    base bytestring containers integer-gmp template-haskell utf8-string
  ];
  testHaskellDepends = [
    base bytestring hspec HUnit QuickCheck quickcheck-instances
    utf8-string
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring gauge integer-gmp megaparsec parsec
    primitive utf8-string
  ];
  homepage = "https://github.com/AndrasKovacs/flatparse#readme";
  description = "High-performance parsing from strict bytestrings";
  license = lib.licenses.mit;
}
;
}
;
  fuzzyfind = {
  meta = {
    sha256 = "17lk2i3gq5kg7h2a4cax6n4lz2mh0qqyrw34lccnwr7nlvpg4var";
    ver = "3.0.1";
  };
  drv = { mkDerivation, base, containers, criterion, deepseq, hspec, lib
, massiv, QuickCheck, text
}:
mkDerivation {
  pname = "fuzzyfind";
  version = "3.0.1";
  src = /nix/store/76z1vv2s7jwnwzf9i949z6qh6if7pbgp-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers massiv text ];
  executableHaskellDepends = [ base criterion deepseq ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  homepage = "http://github.com/runarorama/fuzzyfind/";
  description = "Fuzzy text matching";
  license = lib.licenses.mit;
  mainProgram = "bench";
}
;
}
;
  incipit = {
  meta = {
    sha256 = "13qp45wry6xs54fhkcvydnz9b3nqd88sg1ypg5kpl9af4z9gqd3s";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy-conc
, polysemy-log, polysemy-resume, polysemy-time
}:
mkDerivation {
  pname = "incipit";
  version = "0.9.0.1";
  src = /nix/store/hgi93s4kpm42yha6yba721w2x8szyd6i-source;
  libraryHaskellDepends = [
    base incipit-core polysemy-conc polysemy-log polysemy-resume
    polysemy-time
  ];
  homepage = "https://github.com/tek/incipit#readme";
  description = "A Prelude for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy = {
  meta = {
    sha256 = "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
    ver = "1.9.1.2";
  };
  drv = { mkDerivation, async, base, Cabal, cabal-doctest, containers
, doctest, first-class-families, hspec, hspec-discover
, inspection-testing, lib, mtl, stm, syb, template-haskell
, th-abstraction, transformers, type-errors, unagi-chan
}:
mkDerivation {
  pname = "polysemy";
  version = "1.9.1.2";
  src = /nix/store/anfwczj25hh5zcm9y70vb1221wayi1v0-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    async base containers first-class-families mtl stm syb
    template-haskell th-abstraction transformers type-errors unagi-chan
  ];
  testHaskellDepends = [
    async base containers doctest first-class-families hspec
    hspec-discover inspection-testing mtl stm syb template-haskell
    th-abstraction transformers type-errors unagi-chan
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Higher-order, low-boilerplate free monads";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-chronos = {
  meta = {
    sha256 = "1wvjpl2axxhywjj7z1hjg16sxldq0x63md4rzf1mvdn8067mg35s";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, base, chronos, incipit-core, lib, polysemy-test
, polysemy-time, tasty
}:
mkDerivation {
  pname = "polysemy-chronos";
  version = "0.6.0.2";
  src = /nix/store/4ggkm9nwqlxwix3869ydvhf8dzw0gl4v-source;
  libraryHaskellDepends = [
    base chronos incipit-core polysemy-time
  ];
  testHaskellDepends = [
    base chronos incipit-core polysemy-test polysemy-time tasty
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for Chronos";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-conc = {
  meta = {
    sha256 = "01zfjx1kmrw5hnqyckrwwkdzjbihfn6y516lw7lffhqfp354522b";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, async, base, containers, hedgehog, incipit-core
, lib, polysemy, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, stm, stm-chans, tasty, tasty-hedgehog, time
, torsor, unagi-chan, unix
}:
mkDerivation {
  pname = "polysemy-conc";
  version = "0.13.0.1";
  src = /nix/store/paa9q65s648gd4hxlfipxp5ya96cp237-source;
  libraryHaskellDepends = [
    async base containers incipit-core polysemy polysemy-resume
    polysemy-time stm stm-chans torsor unagi-chan unix
  ];
  testHaskellDepends = [
    async base hedgehog incipit-core polysemy polysemy-plugin
    polysemy-test polysemy-time stm tasty tasty-hedgehog time unix
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for concurrency";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-http = {
  meta = {
    sha256 = "0zg9dhkbsy3sn7gs0axrn4y9z12jqn1138lbcz4lis0s8fjh0zj2";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, aeson, base, case-insensitive, exon, hedgehog
, http-client, http-client-tls, http-types, lib, network, polysemy
, polysemy-plugin, prelate, servant-client, servant-server, tasty
, tasty-hedgehog, time, warp
}:
mkDerivation {
  pname = "polysemy-http";
  version = "0.13.0.1";
  src = /nix/store/1jnlr3wdfc8rzx7a8sbb42bzbcgm39i3-source;
  libraryHaskellDepends = [
    aeson base case-insensitive exon http-client http-client-tls
    http-types polysemy polysemy-plugin prelate time
  ];
  testHaskellDepends = [
    aeson base exon hedgehog http-client network polysemy
    polysemy-plugin prelate servant-client servant-server tasty
    tasty-hedgehog warp
  ];
  homepage = "https://github.com/tek/polysemy-http#readme";
  description = "Polysemy effects for HTTP clients";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-log = {
  meta = {
    sha256 = "1vwlj7xpr4v4340mx8ylfrn2wikix0lkbhg86bikpkzhhk1w3q7q";
    ver = "0.10.0.1";
  };
  drv = { mkDerivation, ansi-terminal, async, base, incipit-core, lib
, polysemy, polysemy-conc, polysemy-plugin, polysemy-test
, polysemy-time, stm, tasty, time
}:
mkDerivation {
  pname = "polysemy-log";
  version = "0.10.0.1";
  src = /nix/store/rjmszzg4h13q7q8sxz1y4z32ay9xqxmn-source;
  libraryHaskellDepends = [
    ansi-terminal async base incipit-core polysemy polysemy-conc
    polysemy-time stm time
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-test polysemy-time tasty time
  ];
  benchmarkHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
  ];
  homepage = "https://github.com/tek/polysemy-log#readme";
  description = "Polysemy effects for logging";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-plugin = {
  meta = {
    sha256 = "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
    ver = "0.4.5.1";
  };
  drv = { mkDerivation, base, Cabal, cabal-doctest, containers, doctest
, ghc, ghc-tcplugins-extra, hspec, hspec-discover
, inspection-testing, lib, polysemy, should-not-typecheck, syb
, transformers
}:
mkDerivation {
  pname = "polysemy-plugin";
  version = "0.4.5.1";
  src = /nix/store/02adx7h7zmis7gay1h0irskrkp7hbql5-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base containers ghc ghc-tcplugins-extra polysemy syb transformers
  ];
  testHaskellDepends = [
    base containers doctest ghc ghc-tcplugins-extra hspec
    hspec-discover inspection-testing polysemy should-not-typecheck syb
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Disambiguate obvious uses of effects";
  license = lib.licenses.bsd3;
}
;
}
;
  polysemy-process = {
  meta = {
    sha256 = "0jzcr0vvmnmpvyyk062lq1k4xcyph9zn6b80wwn6h484qjpwpqcd";
    ver = "0.13.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, path, path-io, polysemy
, polysemy-conc, polysemy-plugin, polysemy-resume, polysemy-test
, polysemy-time, posix-pty, process, stm-chans, tasty
, tasty-expected-failure, typed-process, unix
}:
mkDerivation {
  pname = "polysemy-process";
  version = "0.13.0.1";
  src = /nix/store/6dyq47jsbbh5gh58kjapf35bqll05pk4-source;
  libraryHaskellDepends = [
    base incipit-core path path-io polysemy polysemy-conc
    polysemy-resume polysemy-time posix-pty process stm-chans
    typed-process unix
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-conc polysemy-plugin
    polysemy-resume polysemy-test polysemy-time tasty
    tasty-expected-failure typed-process
  ];
  homepage = "https://github.com/tek/polysemy-conc#readme";
  description = "Polysemy effects for system processes";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-resume = {
  meta = {
    sha256 = "1fci0v1xc6xx8qkj8s57m7yy2w1rxyxvb9bw9vkksdxr3z38dbkg";
    ver = "0.8.0.1";
  };
  drv = { mkDerivation, base, incipit-core, lib, polysemy, polysemy-plugin
, polysemy-test, stm, tasty, transformers
}:
mkDerivation {
  pname = "polysemy-resume";
  version = "0.8.0.1";
  src = /nix/store/h2ayy5qlbfl8qlxa0ivyxq6f0dyhar0v-source;
  libraryHaskellDepends = [
    base incipit-core polysemy transformers
  ];
  testHaskellDepends = [
    base incipit-core polysemy polysemy-plugin polysemy-test stm tasty
  ];
  homepage = "https://github.com/tek/polysemy-resume#readme";
  description = "Polysemy error tracking";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  polysemy-test = {
  meta = {
    sha256 = "1adkp48v04klsjyv8846w7ryf1fiqxb4ga69mps9vg2bp9fj5i7j";
    ver = "0.9.0.0";
  };
  drv = { mkDerivation, base, hedgehog, incipit-core, lib, path, path-io
, polysemy, tasty, tasty-hedgehog, transformers
}:
mkDerivation {
  pname = "polysemy-test";
  version = "0.9.0.0";
  src = /nix/store/jjk10ix28006wr8ljfv4pj6k76xckpsn-source;
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
  polysemy-time = {
  meta = {
    sha256 = "198x2wimdzk93hz0bq2k7wjibcjvzm38m6fica1jfcbh4p531swp";
    ver = "0.6.0.2";
  };
  drv = { mkDerivation, aeson, base, incipit-core, lib, polysemy-test
, tasty, template-haskell, time, torsor
}:
mkDerivation {
  pname = "polysemy-time";
  version = "0.6.0.2";
  src = /nix/store/pflvfr8qg2snw2shhvpwrlz1f51jfyyg-source;
  libraryHaskellDepends = [
    aeson base incipit-core template-haskell time torsor
  ];
  testHaskellDepends = [
    base incipit-core polysemy-test tasty time
  ];
  homepage = "https://github.com/tek/polysemy-time#readme";
  description = "A Polysemy effect for time";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  prelate = {
  meta = {
    sha256 = "0qy0dkckvlbinp1gm85ziiyml0lj57b93qnz23ldjmbj4skcp8s8";
    ver = "0.7.0.1";
  };
  drv = { mkDerivation, aeson, base, exon, extra, generic-lens, incipit
, lib, microlens, microlens-ghc, polysemy-chronos, polysemy-conc
, polysemy-log, polysemy-process, polysemy-resume, polysemy-time
, template-haskell
}:
mkDerivation {
  pname = "prelate";
  version = "0.7.0.1";
  src = /nix/store/b8rilflbwslwc75xamzvkhn5xxnvn6xw-source;
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
  streamly = {
  meta = {
    sha256 = "1w8nhss2rcpbphw2b0d86z7jqxpx38gfn4ahdmd7afg65gzx5bq3";
    ver = "0.8.3";
  };
  drv = { mkDerivation, atomic-primops, base, containers, deepseq
, directory, exceptions, filepath, fusion-plugin-types, ghc-prim
, heaps, lib, lockfree-queue, monad-control, mtl, network
, primitive, transformers, transformers-base, unicode-data
}:
mkDerivation {
  pname = "streamly";
  version = "0.8.3";
  src = /nix/store/vgpkpqqmdrq6860xiw3nvh0nm81ji54p-source;
  libraryHaskellDepends = [
    atomic-primops base containers deepseq directory exceptions
    filepath fusion-plugin-types ghc-prim heaps lockfree-queue
    monad-control mtl network primitive transformers transformers-base
    unicode-data
  ];
  homepage = "https://streamly.composewell.com";
  description = "Dataflow programming and declarative concurrency";
  license = lib.licenses.bsd3;
}
;
}
;
  unicode-data = {
  meta = {
    sha256 = "0pw8cq3spzqdbpyvg026qqyfdva40php7gd728cc56csawpahyx7";
    ver = "0.3.1";
  };
  drv = { mkDerivation, base, deepseq, hspec, hspec-discover, lib, tasty
, tasty-bench
}:
mkDerivation {
  pname = "unicode-data";
  version = "0.3.1";
  src = /nix/store/dgk7n201b9diibh12ajmb12g7dvqs2xm-source;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base deepseq tasty tasty-bench ];
  homepage = "http://github.com/composewell/unicode-data";
  description = "Access Unicode Character Database (UCD)";
  license = lib.licenses.asl20;
}
;
}
;
  zeugma = {
  meta = {
    sha256 = "1clsd2c26cp60kajf4aw8wydnmvgr4blka8yzysi3gzd8ky32ck1";
    ver = "0.9.0.1";
  };
  drv = { mkDerivation, base, chronos, hedgehog, incipit, lib, polysemy
, polysemy-chronos, polysemy-test, tasty, tasty-expected-failure
, tasty-hedgehog
}:
mkDerivation {
  pname = "zeugma";
  version = "0.9.0.1";
  src = /nix/store/l3sj8fw2rjv42a4bgjqbndpmp3f3yadw-source;
  libraryHaskellDepends = [
    base chronos hedgehog incipit polysemy polysemy-chronos
    polysemy-test tasty tasty-expected-failure tasty-hedgehog
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