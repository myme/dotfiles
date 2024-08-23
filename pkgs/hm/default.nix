{ stdenv }:

stdenv.mkDerivation {
  pname = "hm";
  version = "0.1.0";
  src = ./.;
  installPhase = ''
    mkdir -p $out
    cp -av $src/bin $src/share $out
  '';
}
