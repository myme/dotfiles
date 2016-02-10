set background=dark

hi clear

if exists("syntax_on")
    syntax reset
endif

let colors_name = "myme"

hi Normal ctermfg=7 cterm=none guibg=#ffaacc guifg=#660022 gui=none

hi Cursor guibg=#ffbbcc guifg=fg gui=none
hi CursorColumn guibg=#30303a gui=none
hi CursorLine guifg=#330011 guibg=#ff80a0 gui=none
hi DiffAdd guibg=#008b00 guifg=fg gui=none
hi DiffChange guibg=#00008b guifg=fg gui=none
hi DiffDelete guibg=#8b0000 guifg=fg gui=none
hi DiffText guibg=#0000cd guifg=fg gui=bold
hi Directory guibg=bg guifg=#007722 gui=none
hi ErrorMsg ctermfg=1 ctermbg=none cterm=bold guibg=bg guifg=#f07070 gui=bold
hi FoldColumn guibg=bg guifg=#006633 gui=none
hi Folded guibg=#101010 guifg=#c2b680 gui=none
hi Search guibg=#ff80a0 guifg=#330011 gui=none
hi IncSearch guibg=#dd5070 guifg=#110005 gui=none
hi LineNr ctermfg=4 guibg=bg guifg=#662222 gui=none
hi ModeMsg guibg=bg guifg=fg gui=bold
hi MoreMsg guibg=bg guifg=#994466 gui=bold
hi NonText ctermfg=8 guibg=bg guifg=#9f8f80 gui=bold
hi Pmenu ctermbg=5 guibg=#30303a guifg=#c0c0da gui=none
hi PmenuSbar guibg=#B99F86 guifg=fg gui=none
hi PmenuSel ctermbg=4 guibg=#c0c0da guifg=bg gui=none
hi PmenuThumb guibg=#a0a0aa guifg=bg gui=none
hi Question guibg=bg guifg=#007722 gui=bold
hi SignColumn guibg=bg guifg=#7777aa gui=none
hi SpecialKey guibg=bg guifg=#336600 gui=none
if has("spell")
    hi SpellBad guisp=#f07070 gui=undercurl
    hi SpellCap guisp=#7070f0 gui=undercurl
    hi SpellLocal guisp=#70f0f0 gui=undercurl
    hi SpellRare guisp=#f070f0 gui=undercurl
endif
hi StatusLine ctermbg=0 ctermfg=7 cterm=bold guibg=#550000 guifg=#ffaaaa gui=bold
hi StatusLineNC ctermbg=0 ctermfg=4 cterm=bold guibg=#550000 guifg=#cc9999 gui=none
hi TabLine ctermbg=0 ctermfg=7 cterm=bold guibg=#564d43 guifg=#f7f7f1 gui=underline
hi TabLineFill ctermbg=0 ctermfg=7 cterm=bold guibg=#564d43 guifg=#f7f7f1 gui=underline
hi TabLineSel ctermbg=4 ctermfg=7 guibg=bg guifg=#f7f7f1 gui=bold
hi Title ctermbg=0 ctermfg=15 guifg=#f7f7f1 gui=bold
hi VertSplit ctermbg=7 ctermfg=0 guibg=#770044 guifg=#ffaaaa gui=none
if version >= 700
    hi Visual ctermbg=7 ctermfg=0 guibg=#ff80a0 gui=none
else
    hi Visual ctermbg=7 ctermfg=0 guibg=#ff80a0 guifg=fg gui=none
endif
hi VisualNOS guibg=bg guifg=#996633 gui=bold,underline
hi WarningMsg guibg=bg guifg=#f07070 gui=none
hi WildMenu ctermbg=4 ctermfg=7 cterm=bold guibg=fg guifg=bg gui=bold

hi Comment ctermfg=5 guibg=bg guifg=#991166 gui=none
hi Constant ctermfg=1 cterm=bold guibg=bg guifg=#006633 gui=none
hi Error ctermfg=1 ctermbg=none cterm=bold guibg=bg guifg=#b03030 gui=none
hi Identifier ctermfg=4 guibg=bg guifg=#0033aa gui=none
hi Ignore guibg=bg guifg=bg gui=none
hi lCursor guibg=#c0aa94 guifg=bg gui=none
hi MatchParen guibg=#008b8b gui=none
hi PreProc ctermfg=5 guibg=bg guifg=#2277bb gui=none
hi Special ctermfg=4 guibg=bg guifg=#7777aa gui=none
hi Statement ctermfg=2 cterm=bold guibg=bg guifg=#770000 gui=bold
hi Todo guibg=#55cc77 guifg=fg gui=none
hi Type ctermfg=4 cterm=bold guibg=bg guifg=#6655aa gui=bold
hi Underlined ctermfg=3 cterm=bold,underline guibg=bg guifg=#336600 gui=underline

hi htmlBold ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold
hi htmlItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=italic
hi htmlUnderline ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=underline
hi htmlBoldItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,italic
hi htmlBoldUnderline ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,underline
hi htmlBoldUnderlineItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=bold,underline,italic
hi htmlUnderlineItalic ctermbg=0 ctermfg=15 guibg=bg guifg=fg gui=underline,italic

hi ExtraWhitespace ctermbg=2 ctermfg=2 guibg=darkgreen guifg=darkgreen

