{ config, lib, pkgs, ... }:

let
  cfg = config.myme.irc;
  weechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = with pkgs.weechatScripts; [
        wee-slack
        weechat-matrix
      ];
    };
  };
  tmuxCmd = "${pkgs.tmux}/bin/tmux -L weechat";
  weechatCmd = "${weechat}/bin/weechat";
  irc = pkgs.writeShellScriptBin "irc" ''
    # Attach to WeeChat running in a systemd tmux session
    ${tmuxCmd} attach-session -t weechat
  '';

in {
  options = {
    myme.irc.enable = lib.mkEnableOption "Enable IRC (weechat)";
    myme.irc.service = lib.mkEnableOption "Run WeeChat as a user service";
  };

  config = {
    home.packages = lib.mkIf cfg.enable [
      irc
      weechat
    ];

    systemd.user.services.weechat = lib.mkIf (cfg.enable && cfg.service) {
      Unit = {
        Description = "WeeChat - The extensible chat client";
        Documentation = "https://weechat.org/";
      };

      Service = {
        Type = "forking";
        RemainAfterExit = "yes";
        ExecStart = ''${tmuxCmd} new-session -d -s weechat ${weechatCmd}'';
        ExecStop = ''${tmuxCmd} kill-session -t weechat'';
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
