#
# Mail setup
#
# Only the plumbing: the accounts themselves are declared per machine, as
# `accounts.email.accounts.<name>`, and pick up the defaults below. That is
# just what is particular to each one -- address, userName, aliases, IMAP
# host, which one is primary.
#
# Initialize the `mu` index:
#
# $ mu init -m ~/Mail --my-address <address> ...
#

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.mail;

in
{
  options = {
    myme.mail.enable = lib.mkEnableOption "Enable my mail configuration";

    myme.mail.passwordFile = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Path to a JSON file with each account's password, keyed by account name.";
    };

    # Extends home-manager's own account type, rather than wrapping it in one
    # of our own, so a machine can set anything it supports.
    accounts.email.accounts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            config = lib.mkIf cfg.enable {
              realName = lib.mkDefault "Martin Myrseth";
              passwordCommand = lib.mkDefault "${lib.getExe pkgs.jq} -j .${name} ${cfg.passwordFile}";
              mbsync = {
                enable = lib.mkDefault true;
                extraConfig.account = {
                  AuthMechs = lib.mkDefault "LOGIN";
                  Timeout = lib.mkDefault 0;
                };
                create = lib.mkDefault "both";
                expunge = lib.mkDefault "both";
                remove = lib.mkDefault "maildir";
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.mu ];

    accounts.email.maildirBasePath = "Mail";

    programs.mbsync.enable = true;

    services.mbsync = {
      enable = true;
      frequency = "*:0/5";
    };
  };
}
