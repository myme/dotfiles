with import <nixpkgs> {};
mkShell {
  buildInputs = [
    stow
  ];
}
