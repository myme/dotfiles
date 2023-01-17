{ deploy-rs, nixosConfigurations }:

name: host: {
  hostname = host.deploy.host;
  profiles.system = {
    sshUser = nixosConfigurations."${name}".config.myme.machine.user.name;
    sshOpts = if host.deploy ? sshOpts then host.deploy.sshOpts else { };
    magicRollback = false;
    path = deploy-rs.lib."${host.system}".activate.nixos
      nixosConfigurations."${name}";
    user = "root";
  };
}
