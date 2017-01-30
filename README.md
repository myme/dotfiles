# Dotfiles

[GNU stow](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)

`stow --target=$HOME <dir>`


### Issues

ZSH 5.2 has a broken function in `VCS_INFO_nvcsformats`:

```zsh
## vim:ft=zsh
## Written by Frank Terbeck <ft@bewatermyfriend.org>
## Distributed under the same BSD-ish license as zsh itself.

setopt localoptions noksharrays NO_shwordsplit
local c v rr
local -a msgs ### <-- NB: Comment out this line

if [[ $1 == '-preinit-' ]] ; then
    c='default'
    v='-preinit-'
    rr='-all-'
fi
zstyle -a ":vcs_info:${v:-$vcs}:${c:-$usercontext}:${rrn:-$rr}" nvcsformats msgs
(( ${#msgs} > maxexports )) && msgs[${maxexports},-1]=()
return 0
```

Comment out the line as mentioned in the script above to get paths in the prompt working.
