""""""""""""""""""""""""""""""""""""""""""""""""""
" Init
"

" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif
endif


""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"

call plug#begin('~/.vim/plugged')

Plug 'AndrewRadev/linediff.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'SirVer/ultisnips'
Plug 'cakebaker/scss-syntax.vim'
Plug 'digitaltoad/vim-jade'
Plug 'flazz/vim-colorschemes'
Plug 'gregsexton/gitv'
Plug 'groenewege/vim-less'
Plug 'honza/vim-snippets'
Plug 'jamessan/vim-gnupg'
Plug 'jez/vim-superman'
Plug 'kchmck/vim-coffee-script'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'mattn/emmet-vim'
Plug 'mxw/vim-jsx'
Plug 'myusuf3/numbers.vim'
Plug 'othree/xml.vim'
Plug 'pangloss/vim-javascript'
Plug 'rking/ag.vim'
Plug 'scrooloose/syntastic'
Plug 'mtscout6/syntastic-local-eslint.vim'
Plug 'sjl/gundo.vim'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

if has('python3')
    Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer' }
endif

" TODO: Decide on these plugins
" Plug 'cscope_macros'
" Plug 'ghcmod'
" Plug 'haskellmode'
" Plug 'javacomplete'
" Plug 'neco-ghc'
" Plug 'neocomplcache'
" Plug 'vim2hs'
" Plug 'vimclojure'
" Plug 'vimtodo'
" Plug 'zencoding'
" Plug 'scrooloose/nerdtree'

call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""
" Options / setup
"

colorscheme railscasts_myme
let mapleader=","
let maplocalleader=","
let html_use_css=1

set   cmdheight=1
" set   cursorcolumn
" set   cursorline
set   expandtab
set   foldlevel=1
set   foldmethod=syntax
set   laststatus=2
set   modeline
set   number
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

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
endif


""""""""""""""""""""""""""""""""""""""""""""""""""
" Highlight
"

"popup coloring - use mine, not yours
hi Pmenu ctermbg=2 guibg=darkolivegreen
hi PmenuSel ctermbg=0 guibg=black

augroup vimrcEx
au!

autocmd FileType text setlocal textwidth=78

autocmd BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\   exe "normal g`\"" |
\ endif

augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings for "native" Vim commands
"

" Shell + execution
nmap <Leader>x :!./%<Return>
nmap <Leader>sh :sh<Return>
nmap <Leader>!  :!!<Return>

" REPLs
nmap <Leader>ip :!ipython<Return>
nmap <Leader>hs :!ghci<Return>
nmap <Leader>js :!node<Return>
nmap <Leader>cs :!coffee<Return>

" Diff + linediff mapping
nmap <Leader>dt :diffthis<Return>
nmap <Leader>do :diffoff<Return>

" File browser
nmap <Leader>. :30vsplit .<Return>

" Tabs + tags
nmap <Leader>tg <C-w><C-]><C-w>T
nmap <Leader>tc :tabclose<Return>

" Search visual
vmap <Leader>/ y/<C-R>"<Return>

" SnakeCase command
com! -range SnakeCase <line1>,<line2>s!\([A-Z]\)!_\L\1!ge


""""""""""""""""""""""""""""""""""""""""""""""""""
" Options / setup
"

" Ag
vmap <Leader>ag "zy:Ag! -Q "<C-R>z"<CR>
nmap <Leader>ag "zyiw:Ag! -Q "<C-R>z"<CR>


" Airline
let g:airline_powerline_fonts=1
let g:airline_theme="powerlineish"
function! AirlineThemePatch(palette)
  for colors in values(a:palette.inactive)
    let colors[2] = 248
    let colors[3] = 240
  endfor
endfunction
let g:airline_theme_patch_func = 'AirlineThemePatch'


" CtrlP
nmap <Leader>tt :CtrlPTag<Return>
nmap <Leader>bb :CtrlPBookmarkDir<Return>
let g:ctrlp_extensions = [
            \ 'tag', 'buffertag', 'quickfix',
            \ 'line', 'mixed', 'bookmarkdir']
let g:ctrlp_match_func = {'match' : 'pymatcher#PyMatch' }
let g:ctrlp_open_new_file = 't'
let g:ctrlp_root_markers = ['.ctrlp']
let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore node_modules
      \ --ignore cache
      \ --ignore generated
      \ --ignore _build
      \ --ignore .DS_Store
      \ --ignore "**/*.pyc"
      \ -g ""'

let g:ctrlp_buffer_func = { 'enter': 'CtrlPEnter', 'exit':  'CtrlPExit', }

function CtrlPEnter()
  hi! link CursorLine Visual
endfunction

function CtrlPExit()
  hi! link CursorLine NONE
endfunction


" Fugitive
nmap <Leader>gst  :Gst<Return>
nmap <Leader>gd   :Gvdiff<Return>
nmap <Leader>gb   :Gblame<Return>
nmap <Leader>gp   :Git push<Return>
nmap <Leader>gP   :Git push -f<Return>
nmap <Leader>gup  :Git remote update<Return>
nmap <Leader>grup :Git rebase @{u}<Return>
nmap <Leader>gg   "zyiw:Ggrep! "<C-R>z"<CR>


" Gitv
nmap <Leader>gl   :Gitv<Return>
nmap <Leader>gL   :Gitv --all<Return>
nmap <Leader>gf   :Gitv!<Return>
nmap <Leader>gF   :Gitv! --all<Return>


" Gundo
nnoremap <Leader>gun :GundoToggle<CR>


" JSX
let g:jsx_ext_required = 0


" Linediff
vmap <Leader>ldf :Linediff<Return>
nmap <Leader>ldr :LinediffReset<Return>


" Syntastic
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_java_checkers = []
let g:syntastic_javascript_checkers = ["eslint"]


" Tagbar
nmap <Leader>tb :TagbarOpen fjc<Return>
let g:tagbar_type_coffee = {
        \ 'ctagstype' : 'coffee',
        \ 'kinds'     : [
        \ 'c:classes',
        \ 'm:methods',
        \ 'f:functions',
        \ 'v:variables',
        \ 'f:fields',
        \ ]
        \ }


" UltiSnips
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsListSnippets = "<c-l>"
let g:UltiSnipsEditSplit = "horizontal"


" YouCompleteMe
let g:ycm_confirm_extra_conf = 1
