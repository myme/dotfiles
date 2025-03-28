{
  bash,
  fetchzip,
  nodejs,
  writeShellScriptBin,
}:

let
  version = "1.294.0";
  hash = "sha256-adHW/tiiF5KhW0E+O+gGlIGGm05JIwMSh/rOIPy+WnQ=";
  src = fetchzip {
    url = "https://github.com/github/copilot-language-server-release/releases/download/${version}/copilot-language-server-js-${version}.zip";
    inherit hash;
    stripRoot = false;
  };

in
writeShellScriptBin "copilot-language-server" ''${nodejs}/bin/node ${src}/language-server.js "$@"''
