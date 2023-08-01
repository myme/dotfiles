{ inputs, overlays }:

name: host:

let
  inherit (inputs) self agenix home-manager nixpkgs nixos-wsl;

in nixpkgs.lib.nixosSystem {
  inherit (host) system;
  specialArgs = { inherit inputs; };
  modules = [
    ../system
    ../users/root.nix
    agenix.nixosModules.default
    nixos-wsl.nixosModules.wsl
    home-manager.nixosModules.home-manager
    host.config
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
