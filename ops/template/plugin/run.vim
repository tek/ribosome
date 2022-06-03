let s:name = 'plugin'
let s:repo = fnamemodify(expand('<sfile>'), ":p:h:h")
let s:exe = s:repo . '/result/bin/plugin'
let s:cmd = filereadable(s:exe) ? [s:exe] : ['nix', 'run', '.#plugin']
call jobstart(
      \ s:cmd,
      \ { 'rpc': v:true, 'cwd': s:repo, },
      \ )
