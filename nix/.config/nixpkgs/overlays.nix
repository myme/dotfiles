let
  packageSets = self: super: {
    pkgs19_09 = import ./nixpkgs-19.09.nix {};
  };

  base = self: super: {
    baseEnv = self.buildEnv {
      name = "env-base";
      paths = (with self; [
        fzf
      ]) ++ (with super.pkgs19_09; [
        starship
      ]);
    };

    devEnv = self.buildEnv {
      name = "env-development";
      paths = with self; [
        direnv
      ];
    };
  };

in [
  packageSets
  base
]
