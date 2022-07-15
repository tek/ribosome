module Ribosome.App.Boot where

import Exon (exon)
import Rainbow (Chunk, blue, chunk)

import Ribosome.App.Data (Github (Github), GithubOrg (GithubOrg), GithubRepo (GithubRepo), Global, Project (..))
import Ribosome.App.Error (RainbowError)
import Ribosome.App.TemplateTree (writeTemplateTree)
import Ribosome.App.Templates (bootTemplates)
import Ribosome.App.UserInput (fbColor, infoMessage, neovimChunk, pathColor, putStderr)

githubMessage :: [Chunk]
githubMessage =
  [
    "Wrote Github Actions to ",
    pathColor ".github/workflows",
    " that will release static binaries for each commit and tag."
  ]

githubBootMessage :: Github -> [Chunk]
githubBootMessage (Github (GithubOrg org) (GithubRepo repo)) =
  [
    "When ",
    neovimChunk,
    " starts, a binary will be fetched from ",
    fbColor blue 33 (chunk [exon|github.com/#{org}/#{repo}|]),
    " if Nix isn't available."
  ]

generateBoot ::
  Members [Stop RainbowError, Embed IO] r =>
  Global ->
  Project ->
  Sem r ()
generateBoot global Project {..} = do
  writeTemplateTree global directory (bootTemplates (names ^. #name) branch github)
  unless (global ^. #quiet) do
    infoMessage [
      "Wrote ",
      neovimChunk,
      " boot files to ",
      pathColor "plugin/boot.vim",
      "."
      ]
    infoMessage ["Place the project directory in one of your ", pathColor "packpath", " directories to start it."]
    putStderr ""
    for_ github \ gh -> do
      infoMessage githubMessage
      infoMessage (githubBootMessage gh)
