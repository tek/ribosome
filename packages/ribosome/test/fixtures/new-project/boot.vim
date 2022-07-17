let s:repo = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:exe = s:repo . '/result/bin/test-project'
let s:build_cmd = [
  \ 'nix',
  \ '--option', 'extra-substituters', 'https://cach.cachix.org',
  \ '--option', 'extra-trusted-public-keys', '12345',
  \ 'build', '.#test-project',
  \ ]
let s:errors = []

function! s:run(exe) abort "{{{
  call jobstart([a:exe] + get(g:, 'test-project_cli_args', []), { 'rpc': v:true, 'cwd': s:repo, })
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
    call s:error('Failed to build test-project')
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
    call s:error('Failed to fetch the test-project executable from github')
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

let s:gh_exe = s:repo . '/github-exe'
let s:fetch_cmd = [
  \ 'curl',
  \ '--no-progress-meter',
  \ '--location',
  \ '--create-dirs',
  \ '--output',
  \ s:gh_exe,
  \ 'https://github.com/org/rep/releases/download/latest/test-project'
  \ ]

if filereadable(s:exe)
  call s:run(s:exe)
elseif filereadable(s:gh_exe)
  call s:run(s:gh_exe)
else
  if executable('nix') && !get(g:, 'test-project_fetch_bin', 0)
    echo 'Building test-project...'
    call s:nix_build()
  else
    echo 'Fetching the test-project executable from github...'
    call s:fetch_bin()
  endif
endif

