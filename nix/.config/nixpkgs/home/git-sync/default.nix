{ fetchFromGitHub, makeWrapper, stdenv, ... }:

let
  defaultCommitMsg = ''"Auto-commit from \$(uname -n) at \$(date +\"%Y-%m-%d %T\")"'';

in stdenv.mkDerivation {
  pname = "git-sync";
  version = "0d0s33l2..hhjz";
  src = fetchFromGitHub {
    owner = "simonthum";
    repo = "git-sync";
    rev = "2ca0c0253563fb44330c3f95783f4a1cbf1fec35";
    sha256 = "0d0s33l2jyhc02yayysg9n4y650b582vb8nsm1dbh0qk0v6ihhjz";
  };
  buildInputs = [
    makeWrapper
  ];
  patches = [
    ./environmentalize.patch
  ];
  installPhase = ''
    mkdir -p $out/bin
    install git-sync $out/bin
    wrapProgram $out/bin/git-sync \
      --set-default DEFAULT_AUTOCOMMIT_MSG ${defaultCommitMsg}
  '';
}
