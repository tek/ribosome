let s:name = 'plugin'
let s:repo = fnamemodify(expand('<sfile>'), ":p:h:h")
let s:exe = s:repo . '/result/bin/' . s:name
let s:build_cmd = [
      \ 'nix',
      \ '--option', 'extra-substituters', 'https://tek.cachix.org',
      \ '--option', 'extra-trusted-public-keys', 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=',
      \ 'build', '.#' . s:name,
      \ ]

function! s:run() abort "{{{
  call jobstart([s:exe], { 'rpc': v:true, 'cwd': s:repo, })
endfunction "}}}

function! s:built(code) abort "{{{
  if a:code == 0
    call s:run()
  else
    echoerr 'Failed to build ' . s:name
  endif
endfunction "}}}

if filereadable(s:exe)
  call s:run()
else
  echo 'Building ' . s:name . '...'
  call jobstart(s:build_cmd, { 'rpc': v:true, 'cwd': s:repo, 'on_exit': { i, code, t -> s:built(code) } })
endif
