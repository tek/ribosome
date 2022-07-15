module Ribosome.App.Templates.GithubActions where

import Exon (exon)

import Ribosome.App.Data (Branch (Branch), ProjectName (ProjectName))

gaLatest :: ProjectName -> Branch -> Text
gaLatest (ProjectName name) (Branch branch) =
  [exon|---
name: 'latest-release'

on:
  push:
    branches:
      - '#{branch}'

jobs:
  pre-release:
    name: 'Release latest commit'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2.4.0
      - uses: cachix/install-nix-action@v15
        with:
          extra_nix_config: |
            access-tokens = github.com=${{ secrets.GITHUB_TOKEN }}
            extra-substituters = https://tek.cachix.org
            extra-trusted-public-keys = tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=
      - name: 'build'
        run: nix build .#static
      - uses: 'marvinpinto/action-automatic-releases@latest'
        name: 'create release'
        with:
          repo_token: "${{ secrets.GITHUB_TOKEN }}"
          automatic_release_tag: 'latest'
          prerelease: true
          title: 'Executable built from the latest commit'
          files: |
            result/bin/#{name}
|]

gaTags :: ProjectName -> Text
gaTags (ProjectName name) =
  [exon|---
name: "tagged-release"

on:
  push:
    tags:
      - '*'

jobs:
  tagged-release:
    name: 'Tagged Release'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2.4.0
      - uses: cachix/install-nix-action@v15
        with:
          extra_nix_config: |
            access-tokens = github.com=${{ secrets.GITHUB_TOKEN }}
            extra-substituters = https://tek.cachix.org
            extra-trusted-public-keys = tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=
      - name: 'build'
        run: nix build .#static
      - uses: 'marvinpinto/action-automatic-releases@latest'
        name: 'create release'
        with:
          repo_token: "${{ secrets.GITHUB_TOKEN }}"
          prerelease: false
          title: 'Executable built from the latest commit'
          files: |
            result/bin/#{name}
|]
