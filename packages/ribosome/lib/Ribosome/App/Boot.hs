module Ribosome.App.Boot where

import Exon (exon)
import Rainbow (Chunk)

import Ribosome.App.Data (Github (Github), GithubOrg (GithubOrg), GithubRepo (GithubRepo), Global, Project (..))
import Ribosome.App.Error (RainbowError)
import Ribosome.App.TemplateTree (writeTemplateTree)
import Ribosome.App.Templates (bootTemplates)
import Ribosome.App.UserInput (infoMessage, linkChunk, neovimChunk, pathColor, putStderr)

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
    linkChunk [exon|github.com/#{org}/#{repo}|],
    " if Nix isn't available."
  ]

secretsUrl :: Github -> Chunk
secretsUrl (Github (GithubOrg org) (GithubRepo repo)) =
  linkChunk [exon|https://github.com/#{org}/#{repo}/settings/secrets/actions|]

generateBoot ::
  Members [Stop RainbowError, Embed IO] r =>
  Global ->
  Project ->
  Sem r ()
generateBoot global Project {..} = do
  writeTemplateTree global directory (bootTemplates (names ^. #name) branch github cachix)
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
      when (isJust cachix) do
        putStderr ""
        infoMessage ["You have to add your Cachix signing key here: ", secretsUrl gh, "."]
        infoMessage ["The secret name should be ", pathColor "CACHIX_SIGNING_KEY", "."]
        infoMessage ["A key can be created with ", pathColor "cachix generate-keypair", "."]
