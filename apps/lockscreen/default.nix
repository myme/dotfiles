{ i3lock-fancy, writeShellScriptBin }:

writeShellScriptBin "lockscreen" ''
  ${i3lock-fancy}/bin/i3lock-fancy --pixelate
  PID=$!

  while kill ''${PID} -0 ; do
    if timeout 5 fprintd-verify; then
      kill ''${PID}
    fi
  done
''
