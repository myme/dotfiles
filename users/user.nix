{ ... }:
{
  users.users.user = {
    isNormalUser = true;
    hashedPassword =
      "$6$citENBeUFi0e2BS.$fmfM8KgHH3M1dUk4vFwCz.pr9B29Wg1QMWsbbS9r5fhrXTSER8Jx/KHMlSdu9zxW6iBD5teMjM3QwJRfGRXFr/";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [];
  };
}
