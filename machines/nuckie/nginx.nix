{ config, lib, pkgs, ... }:

{
  # Glowing Bear WeeChat Relay Client
  services.nginx = {
    enable = true;
    virtualHosts."nuckie.myme.no" = {
      forceSSL = true;

      locations."^~ /weechat" = {
        proxyPass = "http://127.0.0.1:9001";
        proxyWebsockets = true;
      };

      locations."/".root = pkgs.glowing-bear;

      useACMEHost = "nuckie.myme.no";
    };
  };
}
