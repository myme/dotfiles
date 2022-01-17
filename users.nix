{ ... }: {
  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = null;
      myme = {
        isNormalUser = true;
        hashedPassword = "$6$kmOZ5HPFYnksGRvM$XrteklW/O7XlvQAAhHNwP7hykxNKG/5Fdchp8eXbJ6dON9fBs.EXAwjai7EtjO4JxvVMt57MF/Z5iaoPpk0zm0";
        extraGroups = [ "wheel" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple"
        ];
      };
    };
  };
}
