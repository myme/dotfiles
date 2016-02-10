" File: gtags-cscope.vim
" Author: Tama Communications Corporation
" Version: 0.4
" Last Modified: January 16, 2011
"
" Copyright and license
" ---------------------
" Copyright (c) 2010, 2011 Tama Communications Corporation
"
" This file is part of GNU GLOBAL.
"
" This program is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
" Overview
" --------
" The gtags-cscope.vim plugin script integrates the GNU GLOBAL source code tag system
" with Vim using cscope interface.
"
" Installation
" ------------
" Drop the file in your plugin directory or source it from your vimrc.
" To use this script, you need the GNU GLOBAL-5.8 or later installed
" in your machine.
"
" Usage
" -----
" First of all, you must execute gtags(1) at the root of source directory
" to make tag files. Assuming that your source directory is '/var/src',
" it is neccessary to execute the following commands.
"
" [Load vim]
"	$ cd /var/src
"	$ gtags
"	$ vim
" [Load gtags-cscope]
"	:GtagsCscope <ENTER>            (in vim command line)
"
" Basic command
" -------------
" Then you can use cs commands except for the 'd'(2) command.
" Profitable commands are assigned to keys like follows:
"
"	explanation		command	
"	----------------------------------------------------------
"	Find symbol		:cs find 0 or s
"	Find definition		:cs find 1 or g
"	Find functions called by this function	(not implemented)
"	Find reference		:cs find 3 or c
"	Find text string	:cs find 4 or t
"	Find egrep pattern	:cs find 6 or e
"	Find path		:cs find 7 or f
"	Find include file	:cs find 8 or i
"
" You can move tag list using:
"	Go to the next tag	 :tn
"	Go to the previous tag	 :tp
"	Pop tag stack		 :pop
"
" About the other tag command, you can see the help like this:
"
"          :h tagsrch
"
" Enhancing command
" -----------------
" You can use the context jump function. To use this function, put the cursor
" on a word and type <C-\><C-\><C-]>.
" If you can use mouse then please double click on the left button.
" To pop tag, please type 'g' and click on the right button.
"
" Configure
" ---------
" You can use the following variables in $HOME/.vimrc.
"
" To use the default key/mouse mapping:
"	let GtagsCscope_Auto_Map = 1
" To ignore letter case when searching:
"	let GtagsCscope_Ignore_Case = 1
" To use absolute path name:
"       let GtagsCscope_Absolute_Path = 1
" To deterring interruption:
"	let GtagsCscope_Keep_Alive = 1
" If you hope auto loading:
"	let GtagsCscope_Auto_Load = 1
" To use 'vim -t ', ':tag' and '<C-]>'
"	set cscopetag
"
if exists("loaded_gtags_cscope")
    finish
endif
if !has("cscope")
    call Error('This vim does not include cscope support.')
    finish
endif
"
" global command name
"
let s:global_command = $GTAGSGLOBAL
if s:global_command == ''
        let s:global_command = "global"
endif
if !exists("GtagsCscope_Auto_Load")
    let GtagsCscope_Auto_Load = 0
endif
if !exists("GtagsCscope_Auto_Map")
    let GtagsCscope_Auto_Map = 0
endif
if !exists("GtagsCscope_Use_Old_Key_Map")
    let GtagsCscope_Use_Old_Key_Map = 0
endif
if !exists("GtagsCscope_Quiet")
    let GtagsCscope_Quiet = 0
endif
if !exists("GtagsCscope_Ignore_Case")
    let GtagsCscope_Ignore_Case = 0
endif
if !exists("GtagsCscope_Absolute_Path")
    let GtagsCscope_Absolute_Path = 0
endif
if !exists("GtagsCscope_Keep_Alive")
    let GtagsCscope_Keep_Alive = 0
endif

"
" Display error message.
"
function! s:Error(msg)
    if (g:GtagsCscope_Quiet == 0)
        echohl WarningMsg |
           \ echomsg 'Gtags-cscope: ' . a:msg |
           \ echohl None
    endif
endfunction

function! s:GtagsCscope_GtagsRoot()
    let cmd = s:global_command . " -pq"
    let cmd_output = system(cmd)
    if v:shell_error != 0
        if v:shell_error == 3
            call s:Error('GTAGS not found.')
        else
            call s:Error('global command failed. command line: ' . cmd)
        endif
        return ''
    endif
    return strpart(cmd_output, 0, strlen(cmd_output) - 1)
endfunction

function! s:GtagsCscope()
    "
    " Get gtagsroot directory.
    "
    let gtagsroot = s:GtagsCscope_GtagsRoot()
    if gtagsroot == ''
        return
    endif
    "
    " Load gtags-cscope.
    "
    set csprg=gtags-cscope
    let s:command = "cs add " . gtagsroot . "/GTAGS"
    let s:option = ''
    if g:GtagsCscope_Ignore_Case == 1
        let s:option = s:option . 'C'
    endif
    if g:GtagsCscope_Absolute_Path == 1
        let s:option = s:option . 'a'
    endif
    if g:GtagsCscope_Keep_Alive == 1
        let s:option = s:option . 'i'
    endif
    if s:option != ''
        let s:command = s:command . ' . -' . s:option
    endif
    set nocscopeverbose
    exe s:command
    set cscopeverbose
    "
    " Key mapping
    "
    if g:GtagsCscope_Auto_Map == 1
        if g:GtagsCscope_Use_Old_Key_Map == 1
            " normal command
            :nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR>
            :nmap <C-\>t :cs find g <C-R>=expand("<cword>")<CR>
            :nmap <C-\>r :cs find c <C-R>=expand("<cword>")<CR>
            :nmap <C-\>g :cs find e <C-R>=expand("<cword>")<CR>
            :nmap <C-\>P :cs find f 
            " Using 'CTRL-spacebar', the result is displayed in new horizontal window.
            :nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR>
            :nmap <C-@>t :scs find g <C-R>=expand("<cword>")<CR>
            :nmap <C-@>r :scs find c <C-R>=expand("<cword>")<CR>
            :nmap <C-@>g :scs find e <C-R>=expand("<cword>")<CR>
            :nmap <C-@>P :scs find f 
            " Hitting CTRL-space *twice*, the result is displayed in new vertical window.
            :nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR>
            :nmap <C-@><C-@>t :vert scs find g <C-R>=expand("<cword>")<CR>
            :nmap <C-@><C-@>r :vert scs find c <C-R>=expand("<cword>")<CR>
            :nmap <C-@><C-@>g :vert scs find e <C-R>=expand("<cword>")<CR>
            :nmap <C-@><C-@>P :vert scs find f 
        else
            "
            " The following key mappings are derived from 'cscope_maps.vim'.
            " (The 'd' command is not implemented in gtags-cscope.)
            "
            " normal command
            :nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
            :nmap <C-\>i :cs find i <C-R>=expand("<cfile>")<CR><CR>
            ":nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
            " Using 'CTRL-spacebar', the result is displayed in new horizontal window.
            :nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@>g :scs find g <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@>c :scs find c <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@>t :scs find t <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@>e :scs find e <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
            :nmap <C-@>i :scs find i <C-R>=expand("<cfile>")<CR><CR>
            ":nmap <C-@>d :scs find d <C-R>=expand("<cword>")<CR><CR>
            " Hitting CTRL-space *twice*, the result is displayed in new vertical window.
            :nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@><C-@>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@><C-@>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@><C-@>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@><C-@>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
            :nmap <C-@><C-@>f :vert scs find f <C-R>=expand("<cfile>")<CR><CR>
            :nmap <C-@><C-@>i :vert scs find i <C-R>=expand("<cfile>")<CR><CR>
            ":nmap <C-@><C-@>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>
	endif
	" tag command
	:nmap <C-\><C-n> :tn<CR>
	:nmap <C-\><C-p> :tp<CR>
	:nmap <C-n> :cn<CR>
	:nmap <C-p> :cp<CR>
	" Context search. See the --from-here option of global(1).
	:nmap <C-\><C-\><C-]> :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
	:nmap <2-LeftMouse>   :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
	:nmap g<LeftMouse>    :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
	:nmap <C-LeftMouse>   :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
	" The following mappings are unnecessary, because you can use the default mapping.
	":nmap g<RightMouse>   <C-t>
	":nmap <C-RightMouse>  <C-t>
	" Short cut key
	:nmap <C-\><SPACE> :cs find<SPACE>
	:nmap <C-@><SPACE> :scs find<SPACE>
	:nmap <C-@><C-@><SPACE> :vert scs find<SPACE>
	:nmap <F2> :copen<CR>
	:nmap <F3> :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
	:nmap <F4> :cclose<CR>
    endif
endfunction

if g:GtagsCscope_Auto_Load == 1
    call s:GtagsCscope()
endif
command! -nargs=0 GtagsCscope call s:GtagsCscope()
let loaded_gtags_cscope = 1
