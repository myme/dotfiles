set background=dark

hi clear

if exists("syntax_on")
    syntax reset
endif

let colors_name = "myme"

hi Normal ctermfg=7 cterm=none guibg=#000000 guifg=#ddddff gui=none

hi Cursor guibg=fg guifg=bg gui=none
hi CursorColumn guibg=#30303a gui=none
hi CursorLine guifg=#aaaacc guibg=#30303a gui=none
hi DiffAdd guibg=#008b00 guifg=fg gui=none
hi DiffChange guibg=#00008b guifg=fg gui=none
hi DiffDelete guibg=#8b0000 guifg=fg gui=none
hi DiffText guibg=#0000cd guifg=fg gui=bold
hi Directory guibg=bg guifg=#d4b064 gui=none
hi ErrorMsg ctermfg=1 ctermbg=none cterm=bold guibg=bg guifg=#f07070 gui=bold
hi FoldColumn guibg=bg guifg=#c2b680 gui=none
hi Folded guibg=#101010 guifg=#c2b680 gui=none
hi IncSearch guibg=#866a4f guifg=fg gui=none
hi LineNr ctermfg=4 guibg=bg guifg=#333366 gui=none
hi ModeMsg guibg=bg guifg=fg gui=bold
hi MoreMsg guibg=bg guifg=fg gui=bold
hi NonText ctermfg=8 guibg=bg guifg=#9f8f80 gui=bold
hi Pmenu ctermbg=5 guibg=#30303a guifg=#c0c0da gui=none
hi PmenuSbar guibg=#B99F86 guifg=fg gui=none
hi PmenuSel ctermbg=4 guibg=#c0c0da guifg=bg gui=none
hi PmenuThumb guibg=#a0a0aa guifg=bg gui=none
hi Question guibg=bg guifg=#7777cc gui=bold
hi Search guibg=#20202a guifg=fg gui=none
hi SignColumn guibg=bg guifg=#7777aa gui=none
hi SpecialKey guibg=bg guifg=#d4b064 gui=none
if has("spell")
    hi SpellBad guisp=#f07070 gui=undercurl
    hi SpellCap guisp=#7070f0 gui=undercurl
    hi SpellLocal guisp=#70f0f0 gui=undercurl
    hi SpellRare guisp=#f070f0 gui=undercurl
endif
hi StatusLine ctermbg=0 ctermfg=7 cterm=bold guibg=#20202a guifg=#c0c0da gui=bold
hi StatusLineNC ctermbg=0 ctermfg=4 cterm=bold guibg=#20202a guifg=#c0c0da gui=none
hi TabLine ctermbg=0 ctermfg=7 cterm=bold guibg=#564d43 guifg=#f7f7f1 gui=underline
hi TabLineFill ctermbg=0 ctermfg=7 cterm=bold guibg=#564d43 guifg=#f7f7f1 gui=underline
hi TabLineSel ctermbg=4 ctermfg=7 guibg=bg guifg=#f7f7f1 gui=bold
hi Title ctermbg=0 ctermfg=15 guifg=#77aadd gui=bold
hi VertSplit ctermbg=7 ctermfg=0 guibg=#20202a guifg=#c0c0da gui=none
if version >= 700
    hi Visual ctermbg=7 ctermfg=0 guibg=#5f5f5f gui=none
else
    hi Visual ctermbg=7 ctermfg=0 guibg=#5f5f5f guifg=fg gui=none
endif
hi VisualNOS guibg=bg guifg=#c0aa94 gui=bold,underline
hi WarningMsg guibg=bg guifg=#f07070 gui=none
hi WildMenu ctermbg=4 ctermfg=7 cterm=bold guibg=fg guifg=bg gui=bold

hi Comment ctermfg=5 guibg=bg guifg=#2277bb gui=none
hi Constant ctermfg=1 cterm=bold guibg=bg guifg=#d05050 gui=none
hi Error ctermfg=1 ctermbg=none cterm=bold guibg=bg guifg=#d05050 gui=none
hi Identifier ctermfg=4 guibg=bg guifg=#7777cc gui=none
hi Ignore guibg=bg guifg=bg gui=none
hi lCursor guibg=#c0aa94 guifg=bg gui=none
hi MatchParen guibg=#008b8b gui=none
hi PreProc ctermfg=5 guibg=bg guifg=#2277bb gui=none
hi Special ctermfg=4 guibg=bg guifg=#7777aa gui=none
hi Statement ctermfg=2 cterm=bold guibg=bg guifg=#55aa55 gui=bold
hi Todo guibg=#aed0ae guifg=bg gui=none
hi Type ctermfg=4 cterm=bold guibg=bg guifg=#4070a0 gui=bold
hi Underlined ctermfg=3 cterm=bold,underline guibg=bg guifg=#d4b064 gui=underline

hi htmlBold ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold
hi htmlItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=italic
hi htmlUnderline ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=underline
hi htmlBoldItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,italic
hi htmlBoldUnderline ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,underline
hi htmlBoldUnderlineItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,underline,italic
hi htmlUnderlineItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=underline,italic

hi ExtraWhitespace ctermbg=2 ctermfg=2 guibg=darkgreen guifg=darkgreen

