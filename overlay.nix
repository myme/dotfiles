{
  lib,
  doomemacs,
  wallpapers,
  nixpkgs,
}:

final: prev:

let
  inherit (prev.stdenv.hostPlatform) system;
  unstable = import nixpkgs {
    inherit system;
    config = {
      allowUnfreePredicate =
        pkg:
        builtins.elem pkg.pname [
          "claude-code"
          "github-copilot-cli"
        ];
    };
  };

in
{
  # Always get LLM coding CLIs from unstable
  inherit (unstable) claude-code gemini-cli github-copilot-cli;

  # The 26.05 stable revision's capitaine-cursors isn't on cache.nixos.org
  # (Hydra doesn't build it on the stable channel), so it rebuilds from source
  # via inkscape. Unstable's output is cached and has no runtime deps (pure
  # cursor data), so this is a clean substitution.
  #
  # Cache population isn't visible at eval time, so guard on version drift
  # instead: identical versions are what makes this a *substitution* rather
  # than an upgrade. On unstable hosts `prev` is already `unstable` and this is
  # a quiet no-op.
  capitaine-cursors =
    prev.lib.warnIf (unstable.capitaine-cursors.version != prev.capitaine-cursors.version)
      ''
        capitaine-cursors: unstable (${unstable.capitaine-cursors.version}) has drifted from
        the pinned channel (${prev.capitaine-cursors.version}), so this is no longer a pure
        substitution. Re-check whether the stable build is cached and drop the override in
        overlay.nix.
      ''
      unstable.capitaine-cursors;

  myme = {
    inherit doomemacs wallpapers;
    pkgs =
      let
        vendored = builtins.listToAttrs (
          builtins.map (fname: {
            name = final.lib.strings.removeSuffix ".nix" fname;
            value = final.callPackage ./pkgs/${fname} { };
          }) (lib.myme.allNixFiles ./pkgs)
        );
      in
      vendored
      // {
        # pkgs/dracula-theme.nix is vendored because nixpkgs-unstable dropped
        # dracula-theme on 2026-07-22 along with gtk-engine-murrine. nixos-26.05
        # still ships it, so key the expiry on `unstable` -- that's the channel
        # that forced the vendoring, and it makes the check channel-independent.
        # Removals live on in the alias set as a `throw`, so `?` isn't enough:
        # force the value and catch.
        dracula-theme = prev.lib.warnIf (builtins.tryEval unstable.dracula-theme.name).success ''
          dracula-theme is back in nixpkgs-unstable; drop pkgs/dracula-theme.nix and
          point home-manager/wm/theme.nix at pkgs.dracula-theme.
        '' vendored.dracula-theme;
      };
  };
}
# mailutils 3.21 fails to link on Darwin under libtool 2.6.2: the sieve
# extension modules reference libmailutils symbols without linking against it
# directly, which the two-level namespace linker rejects. Backport of the
# upstream fix (nixpkgs PR #548382), not yet on the nixos-unstable channel.
# Linux links fine and has cached builds, so only patch where it's needed --
# otherwise this forces a source build (in qemu for emulated systems).
# https://github.com/NixOS/nixpkgs/issues/548559
// prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin (
  let
    # Upstream ships the fix as `fix-linking-with-libtool-2.6.2.patch`.
    fixedUpstream = builtins.any (p: prev.lib.hasInfix "libtool" (baseNameOf "${p}")) (
      prev.mailutils.patches or [ ]
    );
  in
  {
    mailutils =
      prev.lib.warnIf fixedUpstream
        ''
          mailutils: nixpkgs now carries the libtool 2.6.2 linking fix. Drop the
          override in overlay.nix and pkgs/mailutils-fix-linking-with-libtool-2.6.2.patch.
        ''
        (
          if fixedUpstream then
            prev.mailutils
          else
            prev.mailutils.overrideAttrs (old: {
              patches = (old.patches or [ ]) ++ [
                ./pkgs/mailutils-fix-linking-with-libtool-2.6.2.patch
              ];
            })
        );
  }
)
