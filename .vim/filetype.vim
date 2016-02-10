if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufRead,BufNewFile .jshintrc		setfiletype json
  au! BufRead,BufNewFile .jscsrc		setfiletype json
augroup END
