{ pkgs, ... }: {
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      plugins = with pkgs.vimPlugins; [
        fzf-vim
        gitv
        vim-airline
        vim-fugitive
        vim-nix
        vim-sensible
        vim-surround
        vim-unimpaired
        vim-vinegar
        # vim-which-key
      ];

      extraConfig = ''
        let mapleader=" "

        " Standard options
        set   cmdheight=1
        set nocursorcolumn
        set nocursorline
        set   laststatus=2
        set   modeline
        set   relativenumber
        set   numberwidth=1
        set   scrolloff=0
        set   showcmd

        " Folds
        set nofoldenable
        set   foldlevel=1
        set   foldmethod=syntax

        " Whitespace
        set   expandtab
        set   shiftwidth=4
        set   tabstop=4
        set nowrap

        " Tab completion
        set   wildchar=<tab>
        set   wildmode=longest:full,full

        " Backup + Swap
        set nobackup
        set noswapfile

        " Git / fugitive
        nmap <silent> <Leader>gg :Git<Return>

        " FZF
        nmap <silent> <Leader>.  :Files<Return>
        nmap <silent> <Leader>g. :GitFiles<Return>
        nmap <silent> <Leader><  :Buffers<Return>
        nmap <silent> <Leader>sb :BLines<Return>
      '';
    };
  };
}
