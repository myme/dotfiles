{ config, lib, pkgs, ... }:

{
  # Glowing Bear WeeChat Relay Client
  services.nginx = {
    enable = true;
    virtualHosts."nuckie.mermaid-map.ts.net" = {
      forceSSL = true;

      locations."^~ /weechat" = {
        proxyPass = "http://127.0.0.1:9001";
        proxyWebsockets = true;
      };

      locations."/".root = pkgs.glowing-bear;

      # enableACME = true;
      sslCertificate = "/var/nginx/nuckie.mermaid-map.ts.net.crt";
      sslCertificateKey = "/var/nginx/nuckie.mermaid-map.ts.net.key";
    };
  };
}
