{ pkgs, python3Packages }:
python3Packages.buildPythonPackage {
  pname = "rofimoji";
  version = "3.0.0-master";
  src = (pkgs.fetchFromGitHub {
    owner = "fdw";
    repo = "rofimoji";
    rev = "master";
    sha256 = "0hm5lcdi8hvx3r2fdl7nh4kx4n2hhcd29cxx4hkhhcbv4n2iwrdd";
  });
  format = "other";
  propagatedBuildInputs = with pkgs; [
    emojione
    rofi
    xdotool
    xorg.xset
  ];
  installPhase = ''
    mkdir -p $out/bin
    install $src/rofimoji.py $out/bin/rofimoji
  '';
}
