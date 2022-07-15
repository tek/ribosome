module Ribosome.App.Templates where

import Exon (exon)
import Path (reldir, relfile)

import Ribosome.App.Data (
  Author,
  Branch,
  FlakeUrl,
  Github,
  Maintainer,
  ProjectName (unProjectName),
  ProjectNames (..),
  Year,
  )
import Ribosome.App.Data.TemplateTree (TemplateTree (TDir, TFile))
import Ribosome.App.Templates.Boot (vimBoot)
import Ribosome.App.Templates.Flake (flakeNix)
import Ribosome.App.Templates.GithubActions (gaLatest, gaTags)
import Ribosome.App.Templates.Hpack (hpackNix)
import Ribosome.App.Templates.License (license)
import Ribosome.App.Templates.MainHs (mainHs)
import Ribosome.App.Templates.PingTestHs (pingTestHs)
import Ribosome.App.Templates.PluginHs (pluginHs)
import Ribosome.App.Templates.TestMainHs (testMainHs)

pluginDir ::
  ProjectName ->
  Maybe Github ->
  TemplateTree
pluginDir name github =
  TDir [reldir|plugin|] [
    TFile [relfile|boot.vim|] (vimBoot name github)
  ]

newProjectTemplates ::
  ProjectNames ->
  FlakeUrl ->
  Author ->
  Maintainer ->
  Year ->
  Maybe Github ->
  TemplateTree
newProjectTemplates ProjectNames {..} flakeUrl author maintainer year github =
  TDir [reldir|.|] [
    TFile [relfile|flake.nix|] (flakeNix flakeUrl name),
    TDir [reldir|packages|] [
      TDir nameDir [
        TFile [relfile|LICENSE|] (license author),
        TFile [relfile|readme.md|] [exon|# #{unProjectName name}|],
        TFile [relfile|changelog.md|] "# Unreleased",
        TFile [relfile|app/Main.hs|] (mainHs moduleName),
        TDir [reldir|lib|] [
          TDir moduleNameDir [
            TFile [relfile|Plugin.hs|] (pluginHs name moduleName)
          ]
        ],
        TDir [reldir|test|] [
          TFile [relfile|Main.hs|] (testMainHs moduleName),
          TDir moduleNameDir [
            TDir [reldir|Test|] [
              TFile [relfile|PingTest.hs|] (pingTestHs moduleName)
            ]
          ]
        ]
      ]
    ],
    TDir [reldir|ops|] [
      TFile [relfile|hpack.nix|] (hpackNix name author maintainer year github),
      TFile [relfile|version.nix|] [exon|"0.1.0.0"|]
    ]
  ]

bootTemplates ::
  ProjectName ->
  Branch ->
  Maybe Github ->
  TemplateTree
bootTemplates name branch github =
  TDir [reldir|.|] [
    pluginDir name github,
    TDir [reldir|.github/workflows|] [
      TFile [relfile|latest.yml|] (gaLatest name branch),
      TFile [relfile|tags.yml|] (gaTags name)
    ]
  ]
