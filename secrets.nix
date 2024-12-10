let
  hostKeys = {
    map = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBjM7kTCk6FHI7Z2EZDGAMOwl/YaIa6aK2+3QwgkiaTN root@map";
    nuckie = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEg58ogznntO3aCpRs8t991VUUyHQvqs2VsuecqkktAv root@nuckie";
    tuple = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMhSCm/KiFfhkTcLaza/GFrpPVEzIFhALxM6gBmNK3Gi root@Tuple";
  };

  userKeys = {
    map = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII1Qsv8MA+cyu7n+4H1kpbVrAmOosJJxjPWAdl08YDvL myme@map";
    nuckie = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMyft4hR6qA+JeB+yMh3uhqXtvJldMc/JAJWpakuI9xD myme@nuckie";
    trie = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIlMUotM7KE9qbVmLQbrp9+gvw8bwtrSU2aEYIG59saC myme@trie";
    tuple = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple";
  };

in
{
  "./secrets/ssh.age".publicKeys = [
    hostKeys.map
    hostKeys.nuckie
    hostKeys.tuple
    userKeys.map
    userKeys.nuckie
    userKeys.tuple
  ];
  "./machines/map/authinfo.age".publicKeys = [
    hostKeys.map
    userKeys.map
  ];
  "./machines/nuckie/acme.age".publicKeys = [
    hostKeys.nuckie
    userKeys.nuckie
  ];
  "./machines/nuckie/weechat.age".publicKeys = [
    hostKeys.nuckie
    userKeys.nuckie
  ];
  "./machines/Tuple/authinfo.age".publicKeys = [
    hostKeys.tuple
    userKeys.tuple
  ];
}
