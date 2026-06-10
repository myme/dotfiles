{
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  settings.global.excludes = [
    "flake.lock"
    "*.age"
    "secrets/*"
    "*.png"
    "*.jpg"
    "*.svg"
  ];
}
