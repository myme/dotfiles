{ gum, stdenv }:

stdenv.mkDerivation {
  pname = "nixos-bootstrap";
  version = "0.0.1";
  src = ./.;
  GUM = "${gum}/bin/gum";
  installPhase = ''
    mkdir -p $out
    cp -av $src/bin $out
    mkdir -p $out/share/nixos-bootstrap
    cp -av $src/lib/* $out/share/nixos-bootstrap
  '';
  fixupPhase = ''
    substituteInPlace $out/bin/* \
        --subst-var GUM \
        --replace-quiet ../lib/setup.sh $out/share/nixos-bootstrap/setup.sh
  '';
}
