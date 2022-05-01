let s:repo = fnamemodify(expand('<sfile>'), ":p:h:h")
let r = jobstart(['nix', 'run', '.#test-plugin'], { 'rpc': v:true, 'cwd': s:repo })
