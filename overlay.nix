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

  agenix = prev.agenix.override {
    # `age` works better than `rage` for editing .age files with SSH keys with
    # passphrases as of 2023-02-16.
    ageBin = "${prev.age}/bin/age";
  };

  # mailutils 3.21 fails to link on aarch64-darwin under libtool 2.6.2: the
  # sieve extension modules reference libmailutils symbols without linking
  # against it directly, which the two-level namespace linker rejects. Backport
  # of the upstream fix (nixpkgs PR #548382), not yet on the nixos-unstable
  # channel. Drop once the channel advances past 2026-08-02.
  # https://github.com/NixOS/nixpkgs/issues/548559
  mailutils = prev.mailutils.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./pkgs/mailutils-fix-linking-with-libtool-2.6.2.patch
    ];
  });

  gnupg240 =
    let
      pname = "gnupg";
      version = "2.4.0";
    in
    prev.gnupg.overrideAttrs {
      inherit pname version;
      src = prev.fetchurl {
        url = "mirror://gnupg/gnupg/${pname}-${version}.tar.bz2";
        hash = "sha256-HXkVjdAdmSQx3S4/rLif2slxJ/iXhOosthDGAPsMFIM=";
      };
    };

  myme = {
    inherit doomemacs wallpapers;
    pkgs = builtins.listToAttrs (
      builtins.map (fname: {
        name = final.lib.strings.removeSuffix ".nix" fname;
        value = final.callPackage ./pkgs/${fname} { };
      }) (lib.myme.allNixFiles ./pkgs)
    );
  };

  # Avoid nvidia vaapi driver collisions with e.g. intel
  # https://github.com/NixOS/nixpkgs/pull/165064
  nvidia-vaapi-driver = prev.lib.hiPrio prev.nvidia-vaapi-driver;
}
