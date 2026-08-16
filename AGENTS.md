# AGENTS.md

Notes for agents (and humans) working in this repo.

## Self-expiring overrides

Source: <https://jezenthomas.com/2026/07/nix-overrides-that-expire-themselves/>

### The problem

Most overrides in this repo are *temporary*: we're routing around a bug, a
missing patch, an uncached build, or a package that upstream dropped. The
condition that justified the override eventually goes away — usually silently,
on a `nix flake update`. Nothing warns you. The override just sits there,
still overriding, quietly costing rebuilds and masking the real package. A
dated `# Drop once …` comment is a note to a human who will never re-read it.

### The trick

Encode the justification as an **eval-time predicate** and wrap the override in
`lib.warnIf`. When the condition flips, the next `nix build` prints a warning
telling you to delete the code — and, where it's safe, transparently falls back
to the unmodified package so nothing breaks in the meantime.

```nix
foo =
  let
    noOverride = lib.versionAtLeast prev.foo.version "1.7.10";
  in
  lib.warnIf noOverride ''
    foo >= 1.7.10 is now in nixpkgs, the override can be removed.
  '' (if noOverride then prev.foo else prev.foo.overrideAttrs { … });
```

Three parts, always:

1. **A predicate** that mechanically re-checks the reason the override exists.
2. **`lib.warnIf` / `lib.warnIfNot`** so the reason surfaces at eval time.
3. **A fallback to `prev.<pkg>`** when the predicate says the override is moot,
   so the warning is informative rather than a build failure waiting to happen.

### Picking a predicate

The predicate should test the *actual* justification, not a proxy for it.

| Why the override exists | Predicate |
| --- | --- |
| Waiting on a version bump | `lib.versionAtLeast prev.foo.version "X.Y"` |
| Waiting on a patch to land | `builtins.any (p: lib.hasInfix "<name>" (baseNameOf "${p}")) (prev.foo.patches or [])` |
| Package marked broken upstream | `lib.warnIfNot prev.foo.meta.broken "…" (markUnbroken prev.foo)` |
| Package removed from nixpkgs (vendored here) | `(builtins.tryEval pkgs.foo.name).success` — nixpkgs removals `throw` from the alias set, so `pkgs ? foo` is **not** enough |
| Pinning around a regression in a moving package | `prev.foo.version != "<version the pin was tested against>"` — warns on *any* drift, i.e. "re-test this" |
| Substituting because a build isn't cached | Not checkable at eval time. Fall back to a drift guard on the version, and keep the dated comment. |

### When *not* to use it

- **Permanent policy**, not a workaround — e.g. always pulling the LLM CLIs
  from `nixpkgs-unstable`. There is no condition to expire.
- **Conditions invisible to eval** — binary-cache population, Hydra jobset
  coverage, hardware quirks. Use a drift guard as a weak proxy or leave a
  comment; don't invent a predicate that doesn't test the real thing.

### Conventions in this repo

- The warning text says **what to delete**, by path, not just "can be removed".
- Keep the prose comment explaining *why* the override exists; the predicate
  explains *when* it dies, not why it was born.
- Prefer the predicate over a dated `# Drop once the channel advances past …`
  comment. Keep the date only where no predicate is possible.
- A silent `if version < X then hack else null` is the anti-pattern this
  replaces — it expires without telling anyone, so dead code accumulates.
  Either wrap it in `warnIf` or delete it.

### Where this is used

- `overlay.nix` — `capitaine-cursors` (channel drift).
- `pkgs/dracula-theme.nix` — vendored copy; warns if nixpkgs re-adds it.
- `machines/list.nix` — `linux-builder` qemu pin; warns when unstable's qemu
  moves off the version the pin was tested against.
