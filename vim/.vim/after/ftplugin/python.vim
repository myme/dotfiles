"autocmd BufWritePost *.py call Flake8()
"autocmd FileType python map <buffer> <F3> :call Flake8()<CR>
"autocmd BufWritePost test_*.py :!nosetests --logging-filter=foo --rednose "%"

nmap <buffer> <Leader>rt :!nosetests --logging-filter=foo --rednose "%"<Return>

let g:syntastic_python_checker_args = '--ignore=E501'
setlocal completeopt-=preview
