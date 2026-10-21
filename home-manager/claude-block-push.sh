# PreToolUse hook (Bash): refuse any `git push` that would update main/master.
#
# The command's own arguments don't tell you where a push lands: with
# push.default=upstream (or a branch that tracks origin/main), a bare
# `git push origin feature` updates main. So resolve it the way git will, by
# re-running each push with --dry-run --porcelain and reading the destination
# refs it reports.
input=$(cat)
command=$(jq -r '.tool_input.command // empty' <<<"$input")
cwd=$(jq -r '.cwd // empty' <<<"$input")

# Fast path: nothing that looks like a push.
grep -Eq '(^|[^[:alnum:]_-])git([[:space:]]+-C[[:space:]]+[^[:space:]]+)?[[:space:]]+push([[:space:]]|$)' <<<"$command" || exit 0

decide() {
  jq -n --arg d "$1" --arg r "$2" \
    '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: $d, permissionDecisionReason: $r}}'
  exit 0
}

# Follow a leading `cd DIR &&` so the dry run sees the same repo.
dir=${cwd:-$PWD}
if [[ $command =~ ^[[:space:]]*cd[[:space:]]+([^[:space:]\;\&\|]+)[[:space:]]*\&\& ]]; then
  target=${BASH_REMATCH[1]/#\~/$HOME}
  [[ $target = /* ]] || target="$dir/$target"
  dir=$target
fi

# Every `git [-C DIR] push ARGS` segment, up to the next ; & | or newline.
while IFS= read -r segment; do
  [[ -n $segment ]] || continue
  pushdir=$dir
  if [[ $segment =~ ^git[[:space:]]+-C[[:space:]]+([^[:space:]]+) ]]; then
    c=${BASH_REMATCH[1]/#\~/$HOME}
    [[ $c = /* ]] || c="$dir/$c"
    pushdir=$c
  fi
  args=$(sed -E 's/^git([[:space:]]+-C[[:space:]]+[^[:space:]]+)?[[:space:]]+push//' <<<"$segment")

  # shellcheck disable=SC2086 # the push's own word splitting is what we want
  # A rejected dry run (non-fast-forward) still lists its refs, so read them
  # before looking at the exit status.
  out=$(cd "$pushdir" 2>/dev/null && eval "git push --dry-run --porcelain $args" 2>&1)
  status=$?
  dsts=$(awk -F'\t' 'NF >= 2 { n = split($2, ref, ":"); print ref[n] }' <<<"$out")
  if grep -Eqx 'refs/heads/(main|master)' <<<"$dsts"; then
    decide deny "Blocked: this push would update $(grep -Ex 'refs/heads/(main|master)' <<<"$dsts" | head -1) directly. Push to a feature branch with an explicit refspec (git push <remote> HEAD:refs/heads/<branch>) and open a PR."
  fi
  if ((status != 0)) && [[ -z $dsts ]]; then
    # Nothing resolved (no network, bad args): that proves nothing, so ask.
    decide ask "Could not resolve where this push lands (git push --dry-run failed): $(tail -n 3 <<<"$out")"
  fi
done < <(grep -Eo 'git([[:space:]]+-C[[:space:]]+[^[:space:]]+)?[[:space:]]+push[^;&|]*' <<<"$command")

exit 0
