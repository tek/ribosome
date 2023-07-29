{ self, config, lib }:
let

  script = config.pkgs.writeScript "ribosome-template-test" ''
    #!${config.pkgs.zsh}/bin/zsh
    setopt err_exit no_unset
    name='ribo-tpl'
    dir=$(mktemp --directory)
    cd $dir
    nix run path:${self}#new -- $name -a Author -m maint@ain.er -o org -r proj -b main --skip-cachix --flake-url 'path:${self}'
    cd $dir/$name
    nix build .#ribo-tpl.static
    if [[ ! -e result/bin/$name ]]
    then
      print "result/bin/$name not generated"
      exit 1
    fi
    print 'success'
  '';

in script
