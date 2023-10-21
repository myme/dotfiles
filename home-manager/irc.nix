{ config, lib, pkgs, ... }:

let
  cfg = config.myme.irc;
  weechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = let ps = pkgs.weechatScripts;
      in [
        ps.wee-slack
        (ps.weechat-matrix.overrideAttrs (attrs:
          attrs // {
            version = "0.3.0-patched";
            src = pkgs.fetchFromGitHub {
              owner = "poljar";
              repo = "weechat-matrix";
              rev = "feae9fda26ea9de98da9cd6733980a203115537e";
              hash = "sha256-flv1XF0tZgu3qoMFfJZ2MzeHYI++t12nkq3jJkRiCQ0=";
            };
            patches = [ ];
          }))
      ];
    };
  };
  tmuxCmd = "${pkgs.tmux}/bin/tmux -L weechat";
  weechatCmd = "${weechat}/bin/weechat";
  irc = pkgs.writeShellScriptBin "irc" ''
    # Attach to WeeChat running in a systemd tmux session
    TMUX_TMPDIR= ${tmuxCmd} attach-session -t weechat
  '';

in {
  options = {
    myme.irc.enable = lib.mkEnableOption "Enable IRC (weechat)";
    myme.irc.service = lib.mkEnableOption "Run WeeChat as a user service";
  };

  config = {
    home.packages = lib.mkIf cfg.enable [ irc weechat ];

    systemd.user.services.weechat = lib.mkIf (cfg.enable && cfg.service) {
      Unit = {
        Description = "WeeChat - The extensible chat client";
        Documentation = "https://weechat.org/";
      };

      Service = {
        Type = "forking";
        RemainAfterExit = "yes";
        ExecStart = "${tmuxCmd} new-session -d -s weechat ${weechatCmd}";
        ExecStop = "${tmuxCmd} kill-session -t weechat";
        Environment = "PATH=${pkgs.coreutils}/bin";
        Restart = "on-failure";
      };

      Install = { WantedBy = [ "default.target" ]; };
    };
  };
}
