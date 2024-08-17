{ gum, jq, stdenv }:

stdenv.mkDerivation {
  pname = "nixos-bootstrap";
  version = "0.0.1";
  src = ./.;
  GUM = "${gum}/bin/gum";
  JQ = "${jq}/bin/jq";
  installPhase = ''
    mkdir -p $out
    cp -av $src/bin $out
    mkdir -p $out/share/nixos-bootstrap
    cp -av $src/lib/* $out/share/nixos-bootstrap
  '';
  fixupPhase = ''
    substituteInPlace $out/bin/* $out/share/nixos-bootstrap/* \
        --subst-var GUM \
        --subst-var JQ \
        --replace-quiet ../lib/setup.sh $out/share/nixos-bootstrap/setup.sh
  '';
}
