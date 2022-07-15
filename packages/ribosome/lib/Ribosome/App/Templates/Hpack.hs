module Ribosome.App.Templates.Hpack where

import Exon (exon)

import Ribosome.App.Data (
  Author (Author),
  Github (Github),
  GithubOrg (GithubOrg),
  GithubRepo (GithubRepo),
  Maintainer (Maintainer),
  ProjectName (ProjectName),
  Year (Year),
  )

githubField ::
  Github ->
  Text
githubField (Github (GithubOrg org) (GithubRepo repo)) =
  [exon|github = "#{repo}/#{org}";|]

hpackNix ::
  ProjectName ->
  Author ->
  Maintainer ->
  Year ->
  Maybe Github ->
  Text
hpackNix (ProjectName name) (Author author) (Maintainer maintainer) (Year year) github =
  [exon|{ config, lib, ... }:
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
    author = "#{author}";
    maintainer = "#{maintainer}";
    copyright = "#{show year} #{author}";
    category = "Neovim";
    build-type = "Simple";
    #{foldMap (githubField) github}
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

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "prelate"; version = ">= 0.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
      "polysemy"
      "polysemy-plugin"
    ];

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

  #{name} = merge (project "#{name}") {
    synopsis = "A Neovim Plugin";
    description = "See https://hackage.haskell.org/package/#{name}/docs/#{name}.html";
    library.dependencies = [
      "ribosome"
      "ribosome-host"
      "ribosome-menu"
    ];
    executables.#{name} = exe "#{name}" "app" {
      dependencies = ["#{name}"];
    };
    tests.#{name}-unit = exe "#{name}" "test" {
      dependencies = [
        "#{name}"
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
|]
