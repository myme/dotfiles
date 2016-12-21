" Vim color file
" Mainteiner: Ruda Moura <ruda@rudix.org>
" Last Change: Mon Jul 17 23:11:05 BRT 2006

" Set 'background' back to the default.  The value can't always be estimated
" and is then guessed.
highlight clear Normal
set background&

" Remove all existing highlighting and set the defaults.
highlight clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

let colors_name = "satori"

" Vim colors
highlight Normal cterm=NONE
highlight Comment ctermfg=Cyan cterm=NONE
highlight Constant ctermfg=Red cterm=NONE
highlight Number ctermfg=Blue cterm=NONE
highlight Identifier ctermfg=NONE cterm=NONE
highlight Statement ctermfg=NONE cterm=Bold
highlight PreProc ctermfg=Yellow cterm=NONE
highlight Type ctermfg=Magenta cterm=NONE
highlight Special ctermfg=Red cterm=Bold
highlight Function ctermfg=Green cterm=NONE

" Vim monochrome
highlight Normal term=NONE
highlight Comment term=NONE
highlight Constant term=Bold
highlight Number term=Bold
highlight Identifier term=NONE
highlight Statement term=Underline
highlight PreProc term=Underline
highlight Type term=NONE
highlight Special term=NONE

" GVim colors
highlight Normal gui=NONE
highlight Comment guifg=DarkCyan gui=NONE
highlight Constant guifg=Red gui=NONE
highlight Number guifg=Blue gui=Bold
highlight Identifier guifg=NONE gui=NONE
highlight Statement guifg=NONE gui=Bold
highlight PreProc guifg=Green gui=NONE
highlight Type guifg=Magenta gui=NONE
highlight Special guifg=Red gui=Bold

" vim: ts=2:sw=2:et
