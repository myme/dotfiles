{ i3lock, imagemagick, scrot, writeShellScriptBin }:

writeShellScriptBin "lockscreen" ''
  bg_image="''${XDG_RUNTIME_DIR:-/tmp}/lock-image.png"

  # Capture screen
  ${scrot}/bin/scrot -o $bg_image

  # Pixelate
  ${imagemagick}/bin/convert -scale 10% -scale 1000% $bg_image $bg_image

  # Lock
  ${i3lock}/bin/i3lock -ni $bg_image

  # PID=$!
  # while kill ''${PID} -0 ; do
  #   if timeout 5 fprintd-verify; then
  #     kill ''${PID}
  #   fi
  # done

  # Remove image on exit
  rm -f $bg_image
''
