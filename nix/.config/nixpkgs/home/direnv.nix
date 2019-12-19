{
  enable = true;
  stdlib = ''
    # -*- mode: shell-script; -*-
    # Based off:
    # https://github.com/direnv/direnv/wiki/Nix#using-a-global-use_nix-with-garbage-collection-prevention
    # https://github.com/Mic92/dotfiles/blob/master/home/.direnvrc
    # XXX: This has now moved to: https://github.com/nix-community/nix-direnv

    use_nix() {
      local path="$(nix-instantiate --find-file nixpkgs)"

      if [ -f "''${path}/.version-suffix" ]; then
        local version="$(< $path/.version-suffix)"
      elif [ -f "''${path}/.git" ]; then
        local version="$(< $(< ''${path}/.git/HEAD))"
      fi

      local cache=".direnv/cache-''${version:-unknown}"

      local update_drv=0
      if [[ ! -e "$cache" ]] || \
        [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
        [[ .envrc -nt "$cache" ]] || \
        [[ default.nix -nt "$cache" ]] || \
        [[ shell.nix -nt "$cache" ]];
      then
        [ -d .direnv ] || mkdir .direnv
        local dump_cmd="echo -n _____direnv_____; \"$direnv\" dump bash"
        local tmp=$(nix-shell --show-trace --pure "$@" \
          --run "$dump_cmd" | grep -oP '(?<=_____direnv_____).*')
        echo "$tmp" > "$cache"
        update_drv=1
      else
        log_status using cached derivation
      fi
      local term_backup=$TERM path_backup=$PATH
      if [ -z ''${TMPDIR+x} ]; then
        local tmp_backup=$TMPDIR
      fi

      log_status eval $cache
      eval "$(< $cache)"
      export PATH=$PATH:$path_backup TERM=$term_backup TMPDIR=$tmp_backup
      if [ -z ''${tmp_backup+x} ]; then
        export TMPDIR=''${tmp_backup}
      else
        unset TMPDIR
      fi

      # `nix-shell --pure` sets invalid ssl certificate paths
      if [ "''${SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
        unset SSL_CERT_FILE
      fi
      if [ "''${NIX_SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
        unset NIX_SSL_CERT_FILE
      fi

      # This part is based on https://discourse.nixos.org/t/what-is-the-best-dev-workflow-around-nix-shell/418/4
      if [ "$out" ] && (( $update_drv )); then
        local drv_link=".direnv/drv"
        local drv="$(nix show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
        local stripped_pwd=''${PWD/\//}
        local escaped_pwd=''${stripped_pwd//-/--}
        local escaped_pwd=''${escaped_pwd//\//-}
        ln -fs "$drv" "$drv_link"
        ln -fs "$PWD/$drv_link" "/nix/var/nix/gcroots/per-user/$LOGNAME/$escaped_pwd"
        log_status renewed cache and derivation link
      fi

      if [[ $# = 0 ]]; then
        watch_file default.nix
        watch_file shell.nix
      fi
    }
  '';
}
