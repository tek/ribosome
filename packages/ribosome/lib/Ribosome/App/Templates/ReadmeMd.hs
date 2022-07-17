module Ribosome.App.Templates.ReadmeMd where

import Exon (exon)

import Ribosome.App.Data (Github (Github), ProjectName (ProjectName), GithubOrg (GithubOrg), GithubRepo (GithubRepo))

githubPlugin :: Github -> Text
githubPlugin (Github (GithubOrg org) (GithubRepo repo)) =
  [exon|
```vim
Plug '#{org}/#{repo}'
```|]

nix :: Text
nix =
  "[Nix package manager](https://nixos.org/learn.html)"

onlyBuild :: Text
onlyBuild =
  [exon|built using the #{nix}, which must be installed in the system.|]

githubFetch :: ProjectName -> Github -> Text
githubFetch (ProjectName name) _ =
  [exon|fetched or built on the first start.

* If the #{nix} is available, the plugin will be fetched from the Nix
  cache (or built if the current commit isn't in the cache)
* Otherwise it will be downloaded from Github's releases.
* If the variable `g:#{name}_fetch_bin` is set to `1`, Nix is ignored and the binary is downloaded from Github
  regardless.
|]

readmeMd ::
  ProjectName ->
  Maybe Github ->
  Text
readmeMd pn@(ProjectName name) github =
  [exon|# Intro
*#{name}* is a Neovim plugin.

# Install

The plugin can be loaded by specifying the repo to a package manager like any other, for example by cloning it in a
subdirectory of `'packpath'` or using one of the many plugin managers.
#{foldMap githubPlugin github}

Since the plugin is written in Haskell with the
[Ribosome](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome.html) framework, its executable has to be
 #{maybe onlyBuild (githubFetch pn) github}
|]
