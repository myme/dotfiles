{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      coc = {
        enable = true;
        settings = {
          "suggest.noselect" = true;
          "suggest.enablePreview" = true;
          "suggest.enablePreselect" = false;
          "suggest.disableKind" = true;
          languageserver = {
            ccls = {
              command = "ccls";
              filetypes = ["c" "cc" "cpp" "c++" "objc" "objcpp"];
              rootPatterns = [".ccls" "compile_commands.json" ".git/" ".hg/"];
              # initializationOptions = {
              #   cache = {
              #     directory = "/tmp/ccls";
              #   };
              # };
            };
            haskell = {
              command = "haskell-language-server-wrapper";
              args = [ "--lsp" ];
              rootPatterns = [
                "*.cabal"
                "stack.yaml"
                "cabal.project"
                "package.yaml"
                "hie.yaml"
              ];
              filetypes = [ "haskell" "lhaskell" ];
            };
          };
        };
      };
      plugins = with pkgs.vimPlugins; [
        # TODO: Add coc stuff
        coc-nvim
        coc-tsserver
        coc-rust-analyzer
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
        set nofoldenable
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
        nmap <silent> <Leader>.  :Files<Return>
        nmap <silent> <Leader>g. :GitFiles<Return>
        nmap <silent> <Leader><  :Buffers<Return>
        nmap <silent> <Leader>sb :BLines<Return>
      '';
    };
  };
}
