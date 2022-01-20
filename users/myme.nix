{ ... }:
{
  users.users.myme = {
    isNormalUser = true;
    hashedPassword =
      "$6$citENBeUFi0e2BS.$fmfM8KgHH3M1dUk4vFwCz.pr9B29Wg1QMWsbbS9r5fhrXTSER8Jx/KHMlSdu9zxW6iBD5teMjM3QwJRfGRXFr/";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple"
      "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAFXxa8PDhPakwNm8+yGX4hW3MFpIJM/vgBt/aVrfLUDkPoh5MPu7nzlZ3xqyfyMKfYy557I6xXPVeR4gikxBI8YhABpVrrL94FJ3yPZc9wwxVQD01vmImO9I3xhTy1YE5ldUFgrVBSjQ2pbn2ARnU5Y1dsI7sS9C0+++sUh4uTuc7VT1A== mmyrseth@set"
    ];
  };
}
