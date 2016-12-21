vmap <buffer> <Leader>ag ""y:Ag! -i "<C-R>""<CR>
nmap <buffer> <Leader>ag ""yiw:Ag! -i "<C-R>""<CR>

fun! XFind(cmd, xpath)
  if empty(a:xpath)
    let l:grepargs = expand("<cword>")
  else
    let l:grepargs = a:xpath . join(a:000, ' ')
  end

  let l:grepprg_bak=&grepprg

  try
    let &grepprg="xfind"
    silent! execute a:cmd . " " . escape(l:grepargs, '|') . " $(ag -gxml$)"
  finally
    let &grepprg=l:grepprg_bak
  endtry

  redraw!

  botright copen
endfun

command! -bang -buffer -nargs=* XFind call XFind('grep<bang>',<q-args>)

vmap <buffer> <Leader>xf ""y:XFind! "//<C-R>""<CR>
nmap <buffer> <Leader>xf ""yiw:XFind! "//<C-R>""<CR>
