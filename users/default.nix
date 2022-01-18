{ ... }: {
  imports = [
    ./myme
  ];

  users = {
    mutableUsers = false;
    users.root.hashedPassword = null;
  };
}
