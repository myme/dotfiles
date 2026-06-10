{ deploy-rs, nixosConfigurations }:

name: host: {
  hostname = host.deploy.host;
  profiles.system = {
    sshUser = nixosConfigurations."${name}".config.myme.machine.user.name;
    sshOpts = host.deploy.sshOpts or { };
    magicRollback = false;
    path = deploy-rs.lib."${host.system}".activate.nixos nixosConfigurations."${name}";
    user = "root";
  };
}
