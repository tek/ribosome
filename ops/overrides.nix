{
dev = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
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
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
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
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
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
};
hix-build-tools = {
};
hls = {
};
min = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
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
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
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
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
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
};
profiled = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
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
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
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
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
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
};
}