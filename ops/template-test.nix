{ self, config, lib }:
let

  inherit (config.devGhc) pkgs;

  script = pkgs.writeScript "ribosome-template-test" ''
    #!${pkgs.zsh}/bin/zsh
    setopt err_exit no_unset
    name='ribo-tpl'
    export RIBOSOME_FLAKE_URL='path:${self}'
    dir=$(mktemp --directory)
    cd $dir
    nix run ''${RIBOSOME_FLAKE_URL}#new -- $name -a Author -m maint@ain.er -o org -r proj -b main
    cd $dir/$name
    nix build .#static
    if [[ ! -e result/bin/$name ]]
    then
      print "result/bin/$name not generated"
      exit 1
    fi
    print 'success'
  '';

in script
