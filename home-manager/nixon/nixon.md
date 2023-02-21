## Generic commands

### `terminal &`

Spawn a terminal.

```bash
x-terminal-emulator
```

### `emacs &`

Start emacs.

```bash
emacs
```

### `org-capture &`

Run `org-capture` from [doomemacs](https://github.com/doomemacs/doomemacs).

```bash
~/.emacs.d/bin/org-capture
```

## nix stuff

### `nix-build`

```bash
nix build
```

### `nix-shell`

```bash
nix develop
```

## npm stuff {type="npm"}

### `npm-scripts` {.json}

List all `npm` scripts in a `package.json`.

```bash
jq '.scripts | to_entries | map({ title: (.key + " → " + .value), value: .key })' package.json
```

### `npm-run ${npm-scripts}`

```bash
npm run "$1"
```

### `npm-install`

```bash
npm install
```

## yarn stuff {type="yarn"}

### `yarn-run ${npm-scripts}`

```bash
yarn run "$1"
```

### `yarn-install`

```bash
yarn install
```

## Cabal stuff {type="cabal"}

### `cabal-build`

```bash
cabal build
```

### `cabal-repl`

```bash
cabal repl
```

### `cabal-run`

```bash
cabal run
```

### `cabal-test`

```bash
cabal test
```

## Git stuff {type="git"}

### `git-log`

```bash
git log --oneline --color
```

### `git-files`

```bash
git ls-files
```

## Files

### `rg-files`

```bash
rg --files
```

### `vim-file ${rg-files}`

```bash
vim "$1"
```

