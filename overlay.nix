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
  inherit (unstable) capitaine-cursors;

  myme = {
    inherit doomemacs wallpapers;
    pkgs = builtins.listToAttrs (
      builtins.map (fname: {
        name = final.lib.strings.removeSuffix ".nix" fname;
        value = final.callPackage ./pkgs/${fname} { };
      }) (lib.myme.allNixFiles ./pkgs)
    );
  };
}
# mailutils 3.21 fails to link on Darwin under libtool 2.6.2: the sieve
# extension modules reference libmailutils symbols without linking against it
# directly, which the two-level namespace linker rejects. Backport of the
# upstream fix (nixpkgs PR #548382), not yet on the nixos-unstable channel.
# Linux links fine and has cached builds, so only patch where it's needed --
# otherwise this forces a source build (in qemu for emulated systems).
# Drop once the channel advances past 2026-08-02.
# https://github.com/NixOS/nixpkgs/issues/548559
// prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
  mailutils = prev.mailutils.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./pkgs/mailutils-fix-linking-with-libtool-2.6.2.patch
    ];
  });
}
