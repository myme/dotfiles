{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      plugins = with pkgs.vimPlugins; [
        # TODO: Add coc stuff
        ale
        fzf-vim
        gitv
        typescript-vim
        vim-airline
        vim-fugitive
        vim-javascript
        vim-plug
        vim-sensible
        vim-surround
        vim-unimpaired
      ];
      extraConfig = ''
        let mapleader=" "

        " Standard options
        set   cmdheight=1
        " set   cursorcolumn
        " set   cursorline
        set   expandtab
        set   foldlevel=1
        set   foldmethod=syntax
        set   laststatus=2
        set   modeline
        set   relativenumber
        set   numberwidth=1
        set   scrolloff=0
        set   shiftwidth=4
        set   showcmd
        set   smartindent
        set   tabstop=4
        set   wildchar=<tab>
        set   wildmode=longest:full,full
        set nobackup
        set noswapfile
        set nowrap

        " FZF
        nmap <silent> <Leader>. :FZF<Return>
      '';
    };
  };
}
