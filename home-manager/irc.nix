{ config, lib, pkgs, ... }:

let
  cfg = config.myme.irc;
  myme-weechat = pkgs.buildEnv {
    name = "weechat";
    paths = with pkgs; [
      weechat
      weechatScripts.wee-slack
      weechatScripts.weechat-matrix
    ];
  };

in {
  options = {
    myme.irc.enable = lib.mkEnableOption "Enable IRC (weechat)";
  };

  config = {
    home.packages = lib.mkIf cfg.enable [
      myme-weechat
    ];
  };
}
