# Bleeding-edge codex, straight from OpenAI's GitHub releases.
#
# Unlike claude-code, nixpkgs builds codex from Rust source -- including a V8
# link step -- so bumping it means a long local rebuild and a fresh
# librusty_v8 pin. Upstream publishes a self-contained `codex-package-*`
# tarball per platform (codex, codex-code-mode-host, plus the rg/bwrap/zsh it
# shells out to) alongside an official codex-package_SHA256SUMS, so installing
# that artifact is both faster and closer to "what upstream ships".
#
# codex locates its bundled helpers relative to the real path of its own
# executable, so the tarball layout is preserved verbatim under libexec and
# $out/bin only holds symlinks into it.
#
# Refresh with ./update.sh.
{
  lib,
  stdenvNoCC,
  fetchurl,
  autoPatchelfHook,
  installShellFiles,
  ncurses,
  versionCheckHook,
  # nixpkgs-unstable, not the host channel -- see the note in claude-code.nix.
  unstable,
}:

let
  release = lib.importJSON ./codex-release.json;

  inherit (stdenvNoCC.hostPlatform) system;
  platform =
    release.platforms.${system} or (throw "codex: upstream publishes no package tarball for ${system}");

  # Same race as claude-code (see llm/claude-code.nix): once nixpkgs draws
  # level, this override only costs us a source-of-truth to keep updated.
  channel = unstable.codex;
  caughtUp = lib.versionAtLeast channel.version release.version;

  bleeding = stdenvNoCC.mkDerivation {
    pname = "codex";
    inherit (release) version;

    src = fetchurl {
      url = "https://github.com/openai/codex/releases/download/${release.tag}/${platform.asset}";
      sha256 = platform.checksum;
    };

    # The tarball has bin/, codex-path/, codex-resources/ and
    # codex-package.json side by side with no single root directory, so it is
    # unpacked straight into its final home. Unpacking it into the build dir
    # instead (`sourceRoot = "."`) would sweep stdenv's own `env-vars` into
    # the copy, and with it a runtime reference to the whole build closure.
    dontUnpack = true;
    dontBuild = true;

    nativeBuildInputs = [
      installShellFiles
    ]
    ++ lib.optional stdenvNoCC.hostPlatform.isElf autoPatchelfHook;

    # Everything but the bundled zsh is static-pie; zsh wants libtinfo.
    buildInputs = lib.optional stdenvNoCC.hostPlatform.isLinux ncurses;

    strictDeps = true;

    # Upstream ships these stripped, and stripping the static-pie binaries
    # again only risks breaking them.
    dontStrip = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/libexec/codex $out/bin
      tar -xzf $src -C $out/libexec/codex
      ln -s $out/libexec/codex/bin/codex $out/bin/codex
      ln -s $out/libexec/codex/bin/codex-code-mode-host $out/bin/codex-code-mode-host

      runHook postInstall
    '';

    postInstall = ''
      installShellCompletion --cmd codex \
        --bash <($out/bin/codex completion bash) \
        --fish <($out/bin/codex completion fish) \
        --zsh <($out/bin/codex completion zsh)
    '';

    doInstallCheck = true;
    nativeInstallCheckInputs = [ versionCheckHook ];
    versionCheckProgramArg = "--version";

    meta = {
      description = "Lightweight coding agent that runs in your terminal (upstream release build)";
      homepage = "https://github.com/openai/codex";
      changelog = "https://github.com/openai/codex/releases/tag/${release.tag}";
      license = lib.licenses.asl20;
      sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
      platforms = builtins.attrNames release.platforms;
      mainProgram = "codex";
    };
  };

in
lib.warnIf caughtUp ''
  codex: nixpkgs-unstable (${channel.version}) has caught up with the release pinned in
  llm/codex-release.json (${release.version}), so the override is no longer
  buying anything. Run llm/update.sh to get ahead again, or drop `codex` from
  overlay.nix and delete llm/codex.nix plus llm/codex-release.json.
'' (if caughtUp then channel else bleeding)
