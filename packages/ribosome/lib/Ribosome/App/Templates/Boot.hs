module Ribosome.App.Templates.Boot where

import Exon (exon)

import Ribosome.App.Data (
  Cachix (Cachix),
  CachixKey (CachixKey),
  CachixName (CachixName),
  Github (Github),
  GithubOrg (GithubOrg),
  GithubRepo (GithubRepo),
  ProjectName (ProjectName),
  cachixTek,
  )

cachixOpts :: Cachix -> Text
cachixOpts (Cachix (CachixName name) (CachixKey key)) =
  [exon|
  \ '--option', 'extra-substituters', 'https://#{name}.cachix.org',
  \ '--option', 'extra-trusted-public-keys', '#{key}',|]

github :: ProjectName -> Github -> Text
github (ProjectName name) (Github (GithubOrg org) (GithubRepo repo)) =
  [exon|let s:gh_exe = s:repo . '/github-exe'
let s:fetch_cmd = [
  \ 'curl',
  \ '--no-progress-meter',
  \ '--location',
  \ '--create-dirs',
  \ '--output',
  \ s:gh_exe,
  \ 'https://github.com/#{org}/#{repo}/releases/download/latest/#{name}'
  \ ]
|]

build :: ProjectName -> Bool -> Text
build (ProjectName name) = \case
  True ->
    [exon|if filereadable(s:exe)
  call s:run(s:exe)
elseif filereadable(s:gh_exe)
  call s:run(s:gh_exe)
else
  if executable('nix') && !get(g:, '#{name}_fetch_bin', 0)
    echo 'Building #{name}...'
    call s:nix_build()
  else
    echo 'Fetching the #{name} executable from github...'
    call s:fetch_bin()
  endif
endif
|]
  False ->
    [exon|if filereadable(s:exe)
  call s:run(s:exe)
else
  if executable('nix')
    echo 'Building #{name}...'
    call s:nix_build()
  else
    echo 'Cannot build #{name} without Nix'
  endif
endif
|]

vimBoot :: ProjectName -> Maybe Github -> Maybe Cachix -> Text
vimBoot pn@(ProjectName name) gh cachix =
  [exon|let s:repo = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:exe = s:repo . '/result/bin/#{name}'
let s:build_cmd = [
  \ 'nix',#{cachixOpts cachixTek}#{foldMap cachixOpts cachix}
  \ 'build', '.##{name}',
  \ ]
let s:errors = []

function! s:run(exe) abort "{{{
  call jobstart([a:exe] + get(g:, '#{name}_cli_args', []), { 'rpc': v:true, 'cwd': s:repo, })
endfunction "}}}

function! s:error(msg) abort "{{{
  call nvim_echo(
        \ [[a:msg . ":\n", 'Error']] +
        \ map(s:errors, { i, s -> [s, 'Error'] }),
        \ v:false, {})
endfunction "}}}

function! s:built(code) abort "{{{
  if a:code == 0
    call s:run(s:exe)
  else
    call s:error('Failed to build #{name}')
  endif
endfunction "}}}

function! s:nix_build() abort "{{{
  call jobstart(s:build_cmd, {
        \ 'cwd': s:repo,
        \ 'on_exit': { i, code, n -> s:built(code) },
        \ 'on_stderr': { i, data, n -> s:stderr(data) },
        \ })
endfunction "}}}

function! s:fetched(code) abort "{{{
  if a:code == 0
    call system(['chmod', '+x',  s:gh_exe])
    call s:run(s:gh_exe)
  else
    call s:error('Failed to fetch the #{name} executable from github')
  endif
endfunction "}}}

function! s:stderr(data) abort "{{{
  call extend(s:errors, a:data)
endfunction "}}}

function! s:fetch_bin() abort "{{{
  call jobstart(s:fetch_cmd, {
        \ 'cwd': s:repo,
        \ 'on_exit': { i, code, n -> s:fetched(code) },
        \ 'on_stderr': { i, data, n -> s:stderr(data) },
        \ })
endfunction "}}}

#{maybe "" (github pn) gh}
#{build pn (isJust gh)}
|]
