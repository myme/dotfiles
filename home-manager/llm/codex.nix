# Codex rewrites ~/.codex/config.toml itself -- trusted projects, model
# selection, TUI nudge counters -- so rendering it with `home.file.text` is the
# wrong shape: the store copy is read-only and activation aborts ("would be
# clobbered") the moment codex has replaced it with its own version.
#
# mkOutOfStoreSymlink points at the checkout instead. Codex writes through the
# symlink in place rather than renaming a temp file over it, so its own edits
# land directly in the repo and show up as a plain `git diff`.
#
# The config lives under machines/<host>/ because most of what codex persists is
# machine-local: trusted project paths, and which repos are checked out where.
#
# This assumes the worktree for this machine sits at
# ~/code/myme/dotfiles/<host>; anywhere else the link dangles and codex falls
# back to its defaults.
{
  config,
  lib,
  osConfig,
  ...
}:

let
  cfg = config.myme.dev;
  host = osConfig.networking.hostName;
  checkout = "${config.home.homeDirectory}/code/myme/dotfiles/${host}";
in
{
  config = lib.mkIf (cfg.llm.enable && cfg.llm.codex) {
    home.file.".codex/config.toml".source =
      config.lib.file.mkOutOfStoreSymlink "${checkout}/machines/${host}/codex-config.toml";
  };
}
