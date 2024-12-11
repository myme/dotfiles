{
  lib,
  stdenv,
  makeWrapper,
  nixon,
  ghc,
  nodejs,
  python313,
}:
stdenv.mkDerivation {
  name = "nixon-with-deps";
  buildInputs = [ makeWrapper ];
  phases = [ "installPhase" ];
  installPhase =
    let
      paths = lib.makeBinPath [
        ghc
        nodejs
        python313
      ];
    in
    ''
      mkdir -p $out/bin
      ln -s ${nixon}/bin/nixon $out/bin/nixon
      ln -s ${nixon}/share $out/share
      wrapProgram $out/bin/nixon --prefix PATH : ${paths}
    '';
}
