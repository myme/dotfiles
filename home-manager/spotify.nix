{ config, lib, pkgs, ... }:

let
  spotify-ctrl = pkgs.writeShellScriptBin "spotify-ctrl" ''
    ${pkgs.dbus}/bin/dbus-send --print-reply \
        --dest=org.mpris.MediaPlayer2.spotify \
        /org/mpris/MediaPlayer2 \
        org.mpris.MediaPlayer2.Player."$1"
  '';

in {
  options = {
    myme.apps.spotify.enable = lib.mkEnableOption "Enable Spotify";
  };

  config = lib.mkIf config.myme.apps.spotify.enable {
    home.packages = with pkgs; [
      spotify
    ] ++ [
      spotify-ctrl
    ];
  };
}
