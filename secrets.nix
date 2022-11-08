let
  hostKeys = {
    map =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILaNxtQ37YaiXRXx+Ff3sPEbzsjA2i934r0Bl+eXVh3P root@map";
    nuckie =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEg58ogznntO3aCpRs8t991VUUyHQvqs2VsuecqkktAv root@nuckie";
    tuple =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMhSCm/KiFfhkTcLaza/GFrpPVEzIFhALxM6gBmNK3Gi root@Tuple";
  };

  userKeys = {
    map =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII1Qsv8MA+cyu7n+4H1kpbVrAmOosJJxjPWAdl08YDvL myme@map";
    nuckie =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMyft4hR6qA+JeB+yMh3uhqXtvJldMc/JAJWpakuI9xD myme@nuckie";
    tuple =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple";
  };

in {
  "./secrets/ssh.age".publicKeys =
    [ hostKeys.map hostKeys.nuckie hostKeys.tuple userKeys.map userKeys.nuckie userKeys.tuple ];
  "./machines/nuckie/weechat.age".publicKeys = [ hostKeys.nuckie userKeys.nuckie ];
}
