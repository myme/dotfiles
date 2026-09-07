# Bleeding-edge LLM CLIs

`claude-code` and `codex` ship several releases a day. `nixpkgs-unstable` is
fast, but "fast" there still means a day or three of lag, which is a lot of
releases for tools that get used all day. These two packages track the
vendors' *own* release channels instead.

Everything else (`gemini-cli`, `github-copilot-cli`, ...) still comes from
`nixpkgs-unstable` via `overlay.nix`.

## How the pins work

Both vendors publish per-platform checksums next to their release artifacts,
so a pin needs no `nix-prefetch` round trip and stays pure:

| tool | upstream source of truth | vendored as |
| --- | --- | --- |
| `claude-code` | `downloads.claude.ai/claude-code-releases/$V/manifest.zst.json` | `claude-code-manifest.json`, verbatim |
| `codex` | `codex-package_SHA256SUMS` on the `rust-v$V` GitHub release | `codex-release.json`, pivoted to nix systems |

`claude-code.nix` is a plain `.override` of nixpkgs' package: nixpkgs already
installs Anthropic's prebuilt binary and accepts the release `manifest` as an
argument, so a newer manifest is the whole change and all the wrapper logic
comes along for free.

`codex.nix` is a `stdenvNoCC.mkDerivation` rather than an override, because
nixpkgs builds codex from Rust source with an embedded V8. See the comment at
the top of that file for why the prebuilt `codex-package-*` tarball wins here.

## The catch-up check

Being ahead of the channel is a race, not a bug fix: nixpkgs-unstable catches
up within days, and from then on the pin is the thing holding us *back*. So
both packages compare their pin against the channel on every eval, and once
`nixpkgs-unstable >= pin` they emit a `lib.warnIf` naming the files to update
or delete, and transparently fall back to the channel package -- the pin can
never leave us on an older build than plain nixpkgs would give.

The comparison is deliberately against **nixpkgs-unstable**, not against the
host's own channel, which is why both files take an `unstable` argument
instead of reaching for `prev`. `machines/rassie.nix` and `machines/deque.nix`
build against nixos-26.05, and a release branch never advances -- a guard
pointed at it would never fire, and the fallback would drop to a build months
behind. The argument is named `unstable` precisely so `callPackage` cannot
auto-fill it from the host package set; getting it wrong is a hard eval error
rather than a silent misconfiguration.

See also the *Self-expiring overrides* section of `../AGENTS.md`.

## Updating

```sh
./update.sh                                       # both, to latest upstream
./update.sh codex                                 # just one
CLAUDE_CODE_VERSION=2.1.263 ./update.sh claude-code   # or pin a specific release
CODEX_TAG=rust-v0.153.4 ./update.sh codex
```

Then rebuild. There is nothing to prefetch and no `flake.lock` entry to bump;
the JSON files *are* the lock.
