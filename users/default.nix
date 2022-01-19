{ ... }: {
  imports = [
    ./myme.nix
  ];

  users = {
    mutableUsers = false;
    users.root.hashedPassword = null;
  };
}
