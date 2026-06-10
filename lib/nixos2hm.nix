{ lib }:

{ nixosConfigurations }:

let
  userAtHostConfig =
    { host, config }:
    (lib.mapAttrsToList (username: hmConfig: {
      name = "${username}@${host}";
      value = hmConfig.home;
    }) config.home-manager.users);

in
with builtins;
(listToAttrs (
  concatMap userAtHostConfig (
    lib.mapAttrsToList (host: config: {
      inherit host;
      inherit (config) config;
    }) nixosConfigurations
  )
))
