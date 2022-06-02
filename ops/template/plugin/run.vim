let s:repo = fnamemodify(expand('<sfile>'), ":p:h:h")
call jobstart(
      \ ['nix', 'run', '.#plugin'],
      \ { 'rpc': v:true, 'cwd': s:repo, },
      \ )
