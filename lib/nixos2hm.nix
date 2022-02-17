{ home-manager, lib, pkgs }:

{ overlays, system, nixosConfigurations }:

let
  removeHostname = str: builtins.head (builtins.split "@" str);
  userAtHostConfig = { host, config }: (
    pkgs.lib.mapAttrsToList
      (username: hmConfig: {
        name = "${username}@${host}";
        value = hmConfig.home;
      })
      config.home-manager.users
  );

in with builtins; (listToAttrs (concatMap userAtHostConfig
  (pkgs.lib.mapAttrsToList (host: config: {
    inherit host;
    inherit (config) config;
  }) nixosConfigurations)))
