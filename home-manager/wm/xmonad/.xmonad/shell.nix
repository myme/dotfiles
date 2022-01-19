{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    xmonad
    xmonad-contrib
  ]);

in pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    ghc
    pkgs.hlint

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
