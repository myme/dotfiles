""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"

" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif
endif


""""""""""""""""""""""""""""""""""""""""""""""""""
"

" Required:
set runtimepath^=~/.vim/bundle/repos/github.com/Shougo/dein.vim
call dein#begin(expand('~/.vim/bundle/'))
call dein#add('Shougo/dein.vim')

if has('python3')
    call dein#add('Valloric/YouCompleteMe', { 'build': './install.py --clang-completer' })
endif

call dein#add('AndrewRadev/linediff.vim')
call dein#add('FelikZ/ctrlp-py-matcher')
call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
call dein#add('SirVer/ultisnips')
call dein#add('cakebaker/scss-syntax.vim')
call dein#add('digitaltoad/vim-jade')
call dein#add('flazz/vim-colorschemes')
call dein#add('gregsexton/gitv')
call dein#add('groenewege/vim-less')
call dein#add('honza/vim-snippets')
call dein#add('jamessan/vim-gnupg')
call dein#add('jez/vim-superman')
call dein#add('kchmck/vim-coffee-script')
call dein#add('kien/ctrlp.vim')
call dein#add('majutsushi/tagbar')
call dein#add('marijnh/tern_for_vim', { 'build': 'npm install' })
call dein#add('mattn/emmet-vim')
call dein#add('mxw/vim-jsx')
call dein#add('myusuf3/numbers.vim')
call dein#add('othree/xml.vim')
call dein#add('pangloss/vim-javascript')
call dein#add('rking/ag.vim')
call dein#add('scrooloose/nerdtree')
call dein#add('scrooloose/syntastic')
call dein#add('mtscout6/syntastic-local-eslint.vim')
call dein#add('sjl/gundo.vim')
call dein#add('tomtom/tcomment_vim')
call dein#add('tpope/vim-fugitive')
call dein#add('tpope/vim-repeat')
call dein#add('tpope/vim-sensible')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-unimpaired')
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')

" TODO: Decide on these plugins
" call dein#add('cscope_macros')
" call dein#add('ghcmod')
" call dein#add('haskellmode')
" call dein#add('javacomplete')
" call dein#add('neco-ghc')
" call dein#add('neocomplcache')
" call dein#add('vim2hs (heads/master)')
" call dein#add('vimclojure')
" call dein#add('vimtodo')
" call dein#add('zencoding')


" Required:
call dein#end()
filetype plugin indent on
if dein#check_install()
    call dein#install()
endif


""""""""""""""""""""""""""""""""""""""""""""""""""
" Options / setup
"

colorscheme railscasts_myme
let mapleader=","
let maplocalleader=","
let html_use_css=1

set   cmdheight=1
set   cursorcolumn
set   cursorline
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
" Key mappings for native Vim commands
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


" NERD Tree options
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=2
nmap <Leader>nt   :NERDTreeToggle<Return>
nmap <Leader>nf   :NERDTreeFind<Return>


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


" VimFiler
let g:vimfiler_tree_opened_icon="▾"
let g:vimfiler_tree_closed_icon="▸"
nmap <Leader>fe :VimFiler -buffer-name=explorer -explorer -find -simple -no-safe -toggle<Return>
nmap <Leader>ff :VimFiler -buffer-name=explorer -no-safe -toggle<Return>
nmap <Leader>fc :VimFilerClose explorer<Return>


" YouCompleteMe
let g:ycm_confirm_extra_conf = 1
