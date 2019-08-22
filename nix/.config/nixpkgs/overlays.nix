let
  base = self: super: {
    baseEnv = self.buildEnv {
      name = "env-base";
      paths = with self; [
      ];
    };
  };

in [
  base
]
