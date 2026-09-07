# Bleeding-edge claude-code, straight from Anthropic's release channel.
#
# nixpkgs' claude-code already installs the official prebuilt binary and takes
# the release `manifest` as an argument, so following upstream is just a matter
# of swapping in a newer manifest -- the wrapper logic (ripgrep, bubblewrap,
# alsa-lib, autoPatchelf) all comes along for free. The manifest carries
# per-platform checksums, so the pin stays pure.
#
# Refresh with ./update.sh.
{
  lib,
  # nixpkgs-unstable, *not* the host's own channel: the stable machines
  # (machines/rassie.nix, machines/deque.nix) build against nixos-26.05, whose
  # release branch never advances, so measuring the pin against it would make
  # the guard below permanently unreachable there. This is also what keeps the
  # unfree predicate in overlay.nix in scope. Deliberately named so that
  # `callPackage` cannot quietly auto-fill it from the host package set.
  unstable,
}:

let
  manifest = lib.importJSON ./claude-code-manifest.json;
  channel = unstable.claude-code;

  # This isn't a workaround waiting on a fix, it's a race: nixpkgs catches up
  # within days and then the "bleeding edge" pin is the one holding us back.
  # Warn the moment that flips, and fall back to the channel so the pin can
  # never leave us on an *older* claude-code than plain nixpkgs would.
  caughtUp = lib.versionAtLeast channel.version manifest.version;

in
lib.warnIf caughtUp ''
  claude-code: nixpkgs-unstable (${channel.version}) has caught up with the
  manifest pinned in llm/claude-code-manifest.json (${manifest.version}), so the
  override is no longer buying anything. Run llm/update.sh to get ahead again,
  or drop `claude-code` from overlay.nix and delete llm/claude-code.nix plus
  llm/claude-code-manifest.json.
'' (if caughtUp then channel else channel.override { inherit manifest; })
