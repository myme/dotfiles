name: machineFile: { inputs, overlays, system }:

let
  inherit (inputs) self agenix home-manager nixpkgs nixos-wsl;

in nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    ../system
    ../users/root.nix
    agenix.nixosModule
    nixos-wsl.nixosModules.wsl
    home-manager.nixosModules.home-manager
    machineFile
    {
      # Hostname
      networking.hostName = name;

      # Let 'nixos-version --json' know about the Git revision
      # of this flake.
      system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

      # Nix + nixpkgs
      nix.registry.nixpkgs.flake = nixpkgs;  # Pin flake nixpkgs
      nixpkgs.overlays = overlays;
    }
  ];
}
