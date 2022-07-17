module Ribosome.App.Templates.GithubActions where

import Exon (exon)

import Ribosome.App.Data (Branch (Branch), Cachix (Cachix), CachixKey (CachixKey), CachixName (CachixName), ProjectName (ProjectName), cachixName, cachixTek)

cachixStep :: CachixName -> Text
cachixStep (CachixName name) =
  [exon|
      - uses: cachix/cachix-action@v10
        with:
          name: #{name}
          signingKey: '${{ secrets.CACHIX_SIGNING_KEY }}'|]

cachixConf :: Cachix -> Text
cachixConf (Cachix (CachixName name) (CachixKey key)) =
  [exon|
            extra-substituters = https://#{name}.cachix.org
            extra-trusted-public-keys = #{key}|]

ga ::
  ProjectName ->
  Maybe Cachix ->
  Text ->
  Text ->
  Text ->
  Text
ga (ProjectName name) cachix actionName push release =
  [exon|---
name: '#{actionName}'

on:
  push:
    #{push}

jobs:
  release:
    name: 'Release'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2.4.0
      - uses: cachix/install-nix-action@v15
        with:
          extra_nix_config: |
            access-tokens = github.com=${{ secrets.GITHUB_TOKEN }}#{cachixConf (fromMaybe cachixTek cachix)}#{foldMap (cachixStep . cachixName) cachix}
      - name: 'build'
        run: nix build .#static
      - uses: 'marvinpinto/action-automatic-releases@latest'
        name: 'create release'
        with:
          repo_token: "${{ secrets.GITHUB_TOKEN }}"
          #{release}
          files: |
            result/bin/#{name}
|]

gaLatest ::
  ProjectName ->
  Branch ->
  Maybe Cachix ->
  Text
gaLatest name (Branch branch) cachix =
  ga name cachix "latest-release" [exon|branches:
      - '#{branch}'|] [exon|automatic_release_tag: 'latest'
          prerelease: true|]

gaTags ::
  ProjectName ->
  Maybe Cachix ->
  Text
gaTags name cachix =
  ga name cachix "tagged-release" [exon|tags:
      - '*'|] [exon|prerelease: false|]
