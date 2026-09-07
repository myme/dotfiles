#!/usr/bin/env nix
#!nix shell --ignore-environment nixpkgs#bash nixpkgs#cacert nixpkgs#coreutils nixpkgs#curl nixpkgs#jq --command bash

# Refresh the upstream release pins for the LLM CLIs in ./ -- see README.md.
#
#   ./update.sh                                    both tools, latest upstream
#   ./update.sh claude-code                        just one
#   CLAUDE_CODE_VERSION=2.1.263 ./update.sh claude-code
#   CODEX_TAG=rust-v0.153.4 ./update.sh codex

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

update_claude_code() {
  local base=https://downloads.claude.ai/claude-code-releases
  local version=${CLAUDE_CODE_VERSION:-$(curl -fsSL "$base/latest")}

  # The manifest is upstream's own artifact and already carries a checksum per
  # platform, so it is vendored verbatim and needs no rewriting.
  curl -fsSL "$base/$version/manifest.zst.json" -o claude-code-manifest.json
  echo "claude-code -> $(jq -r .version claude-code-manifest.json)"
}

update_codex() {
  local api=https://api.github.com/repos/openai/codex
  local tag=${CODEX_TAG:-$(curl -fsSL "$api/releases/latest" | jq -r .tag_name)}
  local sums
  sums=$(curl -fsSL "https://github.com/openai/codex/releases/download/$tag/codex-package_SHA256SUMS")

  # Upstream ships no single manifest to vendor, so pivot its own SHA256SUMS
  # into one, keyed by nix system rather than by rust target triple.
  jq -n \
    --arg tag "$tag" \
    --arg sums "$sums" \
    '
      {
        "x86_64-linux":   "x86_64-unknown-linux-musl",
        "aarch64-linux":  "aarch64-unknown-linux-musl",
        "x86_64-darwin":  "x86_64-apple-darwin",
        "aarch64-darwin": "aarch64-apple-darwin"
      } as $targets
      | ($sums
          | split("\n")
          | map(select(length > 0) | split("  ") | { key: .[1], value: .[0] })
          | from_entries) as $checksums
      | {
          version: ($tag | ltrimstr("rust-v")),
          tag: $tag,
          platforms: (
            $targets
            | with_entries(
                ("codex-package-" + .value + ".tar.gz") as $asset
                | select($checksums[$asset] != null)
                | .value = { asset: $asset, checksum: $checksums[$asset] }
              )
          )
        }
    ' > codex-release.json
  echo "codex -> $(jq -r .version codex-release.json) ($(jq -r '.platforms | keys | join(", ")' codex-release.json))"
}

tools=("$@")
[[ ${#tools[@]} -gt 0 ]] || tools=(claude-code codex)

for tool in "${tools[@]}"; do
  case "$tool" in
    claude-code) update_claude_code ;;
    codex) update_codex ;;
    *)
      echo "unknown tool: $tool (expected claude-code or codex)" >&2
      exit 1
      ;;
  esac
done
