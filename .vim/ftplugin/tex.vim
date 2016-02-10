" Label
imap <buffer> <C-l>l <Esc>bi\label{<Esc>ea}

" Citations
imap <buffer> <C-l>c <Esc>Bi\cite{<Esc>Ea}

" References
imap <buffer> <C-l><C-r>t <Esc>bi\ref{table:<Esc>ea}
imap <buffer> <C-l><C-r>f <Esc>bi\ref{figure:<Esc>ea}

" Acronym
map  <buffer> <C-l>a bi\ac{<Esc>ea}<Esc>
imap <buffer> <C-l>a <Esc>bi\ac{<Esc>ea}
map  <buffer> <C-l>p bi\acp{<Esc>ea}<Esc>
imap <buffer> <C-l>p <Esc>bi\acp{<Esc>ea}

" Sections
imap <buffer> <C-l>S \section{}<Esc>i
imap <buffer> <C-l>s \subsection{}<Esc>i

" Images, lists, tables, etc.
" imap <buffer> <expr> <C-l>p ImageTemplate()
" imap <buffer> <expr> <C-l>t TableTemplate()

" " Return an image template
" function ImageTemplate()
"     return "\\begin{figure}[!h]\n\\centering\n\\epsfig{file=,width=cm}\n\\caption{\\textit{}}\n\\label{fig:}\n\\end{figure}"
" endfunction
" 
" " Return a table template
" function TableTemplate()
"     return "\\begin{table}\n\\begin{center}\n\\begin{tabular}{||}\\hline\n1 & 2\n\\end{tabular}\n\\end{center}\n\\caption{}\n\\label{table:cheat_classification}\n\\end{table}"
" endfunction

set nosmartindent

