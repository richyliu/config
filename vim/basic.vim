" Basic vimrc all in one file

" Quick install commmand for Unix:
" curl https://raw.githubusercontent.com/richyliu/vimrc/master/vim/basic.vim -o myvimrc.vim
"
" Usage:
" vim -u myvimrc.vim [FILE]

set nocompatible

colorscheme zellner

" Settings {{{1
set autoindent
set background=light
set backspace=indent,eol,start
set clipboard=
set cursorline
set expandtab
set foldmethod=marker
set hidden
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set linebreak
set nomodeline
set mouse=a
set nrformats-=octal
set number
set path=.,,**
set relativenumber
set ruler
set scrolloff=5
set sidescroll=5
set sidescrolloff=10
set shiftwidth=2
set noshelltemp
set showcmd
set smartcase
set softtabstop=2
set spellcapcheck=
set splitbelow
set splitright
set nostartofline
set tabstop=4
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=10
set updatetime=500
set wildignore+=**/node_modules/**
set wildignorecase
set wildmenu
set wildmode=longest:full,full

filetype plugin on
syntax on

let g:buftabline_indicators=1
let g:buftabline_numbers=2

" }}}1

" Keymaps {{{1
let mapleader = " "
let maplocalleader = "Q"
" Save file
noremap <leader>w :w<cr>
" Source current file
noremap <leader>o :source %<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open file in current folder
nnoremap <leader>e :e <c-d>
nnoremap <leader>ee :e <c-d>
" Open file
nnoremap <leader>f :find<space>
" cd to current file directory
nnoremap <leader>c :cd %:p:h<cr>
" change to paste mode
nnoremap <leader>a :set paste!<cr>
" toggle relative number
nnoremap <leader>l :setlocal relativenumber!<cr>
" toggle line wrapping
nnoremap <leader>p :setlocal wrap!<cr>
" open help search
nnoremap <leader>h :help<space>
" enable verymagic for regex
cnoremap <c-o> <c-f>^f/a\v<c-c><c-e>
" change settings
nnoremap <leader>u :set<space>
" run external shell command
nnoremap ! :!
" Make ex mode harder to enter on accident
nnoremap Q :echo "To enter Ex mode, type  gQ  or start vim with 'vim -e'"<cr>
" switch buffers
nnoremap <leader>b :ls<cr>:b
" search
nnoremap <leader>% :%s/
vnoremap <leader>% :s/
" search repeats
nnoremap <leader>g :%g/
vnoremap <leader>g :g/
" yank to system clipboard
nnoremap <leader>y "+y
vnoremap <leader>y "+y
" close all vim buffers
nnoremap ZA :qa!<cr>

" Move line up or down
nnoremap _ :silent! .m.-2<cr>
nnoremap + :silent! .m+<cr>
" Yank until end of line
nnoremap Y y$

" Delete with Ctrl-L (backspace w/ Ctrl-H)
inoremap <c-l> <del>
" Delete line in insert mode
inoremap <c-d> <c-o>dd
" Un-indent with Ctrl-F (b/c Ctrl-D doesn't work)
inoremap <c-f> <c-d>
" Go to previous command with Ctrl-P
nnoremap <c-p> :<c-p>
vnoremap <c-p> :<c-p>
" Go to beginning of line with ctrl-a
cnoremap <c-a> <c-b>
" Make ctrl-u start a separate history entry
inoremap <c-u> <c-g>u<c-u>

" Make { and } always work linewise
onoremap { V{
onoremap } V}

" Search for selected text and automatically escape characters
" Source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(escape(@", '/\.*$^~['),'\n','\\n','g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(escape(@", '?\.*$^~['),'\n','\\n','g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

" Operator pending mappings
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap an( :<c-u>normal! f(va(<cr>
onoremap in) :<c-u>normal! f)vi)<cr>
onoremap an) :<c-u>normal! f)va)<cr>
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap an{ :<c-u>normal! f{va{<cr>
onoremap in} :<c-u>normal! f}vi}<cr>
onoremap an} :<c-u>normal! f}va}<cr>
onoremap in' :<c-u>normal! f'vi'<cr>
onoremap an' :<c-u>normal! f'va'<cr>
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap an" :<c-u>normal! f"va"<cr>

" Buffer switching
nmap <leader>1 :<c-u>BufTabLineGo1<cr>
nmap <leader>2 :<c-u>BufTabLineGo2<cr>
nmap <leader>3 :<c-u>BufTabLineGo3<cr>
nmap <leader>4 :<c-u>BufTabLineGo4<cr>
nmap <leader>5 :<c-u>BufTabLineGo5<cr>
nmap <leader>6 :<c-u>BufTabLineGo6<cr>
nmap <leader>7 :<c-u>BufTabLineGo7<cr>
nmap <leader>8 :<c-u>BufTabLineGo8<cr>
nmap <leader>9 :<c-u>BufTabLineGo20<cr>

" }}}1

" Editing GPG files {{{1

" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff
" Source: https://vim.fandom.com/wiki/Encryption
" Modified slightly by me (Richard Liu)
augroup encrypted
  au!

  " First make sure nothing is written to ~/.viminfo while editing
  " an encrypted file.
  autocmd BufReadPre,FileReadPre *.gpg set viminfo=
  " We don't want a various options which write unencrypted data to disk
  autocmd BufReadPre,FileReadPre *.gpg set noswapfile noundofile nobackup nowritebackup

  " Switch to binary mode to read the encrypted file
  autocmd BufReadPre,FileReadPre *.gpg let ch_save = &ch|set ch=2
  " (If you use tcsh, you may need to alter this line.)
  autocmd BufReadPost,FileReadPost *.gpg '[,']!gpg --decrypt 2> /dev/null

  " Switch to normal mode for editing
  autocmd BufReadPost,FileReadPost *.gpg set nobin
  autocmd BufReadPost,FileReadPost *.gpg let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost *.gpg execute ":doautocmd BufReadPost " . expand("%:r")

  " Use a mark to remember where we are
  autocmd BufWritePre,FileWritePre *.gpg execute "normal! mz"
  " Convert all text to encrypted text before writing
  autocmd BufWritePre,FileWritePre *.gpg '[,']!gpg --default-recipient-self -ae 2>/dev/null
  " Undo the encryption so we are back in the normal text after the file has been written.
  autocmd BufWritePost,FileWritePost *.gpg undo
  " Go back to the mark and delete it
  autocmd BufWritePost,FileWritePost *.gpg execute "normal! `z"
  autocmd BufWritePost,FileWritePost *.gpg delmark z

  " Disable gitgutter for gpg encrypted files
  autocmd BufReadPost,FileReadPost *.gpg let g:gitgutter_enabled = 0
augroup END

" }}}1

" Statusline {{{1
" https://github.com/Greduan/dotfiles/blob/76e16dd8a04501db29989824af512c453550591d/vim/after/plugin/statusline.vim

let g:currentmode={
      \ 'n'  : 'NORMAL ',
      \ 'no' : '(INSERT) ',
      \ 'v'  : 'VISUAL ',
      \ 'V'  : 'V-LINE ',
      \ '' : 'V-Block ',
      \ 's'  : 'SELECT ',
      \ 'S'  : 'S-LINE ',
      \ '' : 'S-BLOCK ',
      \ 'i'  : 'INSERT ',
      \ 'R'  : 'REPLACE ',
      \ 'Rv' : 'V REPLACE ',
      \ 'c'  : 'COMMAND ',
      \ 'cv' : 'VIM EX ',
      \ 'ce' : 'EX ',
      \ 'r'  : 'PROMPT ',
      \ 'rm' : 'MORE ',
      \ 'r?' : 'CONFIRM ',
      \ '!'  : 'SHELL ',
      \ 't'  : 'TERMINAL '
      \}

set laststatus=2
set statusline=
set statusline+=%0*\ %{g:currentmode[mode()]}             " Current mode
set statusline+=%1*\ [%n]                                 " Buffer number
set statusline+=%1*\ %<%F\ %m\ %w\                        " File path
set statusline+=%1#warningmsg#
set statusline+=%1*
set statusline+=%1*\ %=                                   " Space
set statusline+=%1*\ %y\                                  " FileType
set statusline+=%1*\ %{(&fenc!=''?&fenc:&enc)}\[%{&ff}]\  " Encoding & Fileformat
set statusline+=%0*\ %3p%%\ %l\/%L\ ln:%3c\               " Line and column numbers

" }}}1



" surround.vim - Surroundings {{{1
" Author:       Tim Pope <http://tpo.pe/>
" Version:      2.1
" GetLatestVimScripts: 1697 1 :AutoInstall: surround.vim

if exists("g:loaded_surround") || &cp || v:version < 700
  finish
endif
let g:loaded_surround = 1

" Input functions {{{2

function! s:getchar()
  let c = getchar()
  if c =~ '^\d\+$'
    let c = nr2char(c)
  endif
  return c
endfunction

function! s:inputtarget()
  let c = s:getchar()
  while c =~ '^\d\+$'
    let c .= s:getchar()
  endwhile
  if c == " "
    let c .= s:getchar()
  endif
  if c =~ "\<Esc>\|\<C-C>\|\0"
    return ""
  else
    return c
  endif
endfunction

function! s:inputreplacement()
  let c = s:getchar()
  if c == " "
    let c .= s:getchar()
  endif
  if c =~ "\<Esc>" || c =~ "\<C-C>"
    return ""
  else
    return c
  endif
endfunction

function! s:beep()
  exe "norm! \<Esc>"
  return ""
endfunction

function! s:redraw()
  redraw
  return ""
endfunction

" }}}2

" Wrapping functions {{{2

function! s:extractbefore(str)
  if a:str =~ '\r'
    return matchstr(a:str,'.*\ze\r')
  else
    return matchstr(a:str,'.*\ze\n')
  endif
endfunction

function! s:extractafter(str)
  if a:str =~ '\r'
    return matchstr(a:str,'\r\zs.*')
  else
    return matchstr(a:str,'\n\zs.*')
  endif
endfunction

function! s:fixindent(str,spc)
  let str = substitute(a:str,'\t',repeat(' ',&sw),'g')
  let spc = substitute(a:spc,'\t',repeat(' ',&sw),'g')
  let str = substitute(str,'\(\n\|\%^\).\@=','\1'.spc,'g')
  if ! &et
    let str = substitute(str,'\s\{'.&ts.'\}',"\t",'g')
  endif
  return str
endfunction

function! s:process(string)
  let i = 0
  for i in range(7)
    let repl_{i} = ''
    let m = matchstr(a:string,nr2char(i).'.\{-\}\ze'.nr2char(i))
    if m != ''
      let m = substitute(strpart(m,1),'\r.*','','')
      let repl_{i} = input(match(m,'\w\+$') >= 0 ? m.': ' : m)
    endif
  endfor
  let s = ""
  let i = 0
  while i < strlen(a:string)
    let char = strpart(a:string,i,1)
    if char2nr(char) < 8
      let next = stridx(a:string,char,i+1)
      if next == -1
        let s .= char
      else
        let insertion = repl_{char2nr(char)}
        let subs = strpart(a:string,i+1,next-i-1)
        let subs = matchstr(subs,'\r.*')
        while subs =~ '^\r.*\r'
          let sub = matchstr(subs,"^\r\\zs[^\r]*\r[^\r]*")
          let subs = strpart(subs,strlen(sub)+1)
          let r = stridx(sub,"\r")
          let insertion = substitute(insertion,strpart(sub,0,r),strpart(sub,r+1),'')
        endwhile
        let s .= insertion
        let i = next
      endif
    else
      let s .= char
    endif
    let i += 1
  endwhile
  return s
endfunction

function! s:wrap(string,char,type,removed,special)
  let keeper = a:string
  let newchar = a:char
  let s:input = ""
  let type = a:type
  let linemode = type ==# 'V' ? 1 : 0
  let before = ""
  let after  = ""
  if type ==# "V"
    let initspaces = matchstr(keeper,'\%^\s*')
  else
    let initspaces = matchstr(getline('.'),'\%^\s*')
  endif
  let pairs = "b()B{}r[]a<>"
  let extraspace = ""
  if newchar =~ '^ '
    let newchar = strpart(newchar,1)
    let extraspace = ' '
  endif
  let idx = stridx(pairs,newchar)
  if newchar == ' '
    let before = ''
    let after  = ''
  elseif exists("b:surround_".char2nr(newchar))
    let all    = s:process(b:surround_{char2nr(newchar)})
    let before = s:extractbefore(all)
    let after  =  s:extractafter(all)
  elseif exists("g:surround_".char2nr(newchar))
    let all    = s:process(g:surround_{char2nr(newchar)})
    let before = s:extractbefore(all)
    let after  =  s:extractafter(all)
  elseif newchar ==# "p"
    let before = "\n"
    let after  = "\n\n"
  elseif newchar ==# 's'
    let before = ' '
    let after  = ''
  elseif newchar ==# ':'
    let before = ':'
    let after = ''
  elseif newchar =~# "[tT\<C-T><]"
    let dounmapp = 0
    let dounmapb = 0
    if !maparg(">","c")
      let dounmapb = 1
      " Hide from AsNeeded
      exe "cn"."oremap > ><CR>"
    endif
    let default = ""
    if newchar ==# "T"
      if !exists("s:lastdel")
        let s:lastdel = ""
      endif
      let default = matchstr(s:lastdel,'<\zs.\{-\}\ze>')
    endif
    let tag = input("<",default)
    if dounmapb
      silent! cunmap >
    endif
    let s:input = tag
    if tag != ""
      let keepAttributes = ( match(tag, ">$") == -1 )
      let tag = substitute(tag,'>*$','','')
      let attributes = ""
      if keepAttributes
        let attributes = matchstr(a:removed, '<[^ \t\n]\+\zs\_.\{-\}\ze>')
      endif
      let s:input = tag . '>'
      if tag =~ '/$'
        let tag = substitute(tag, '/$', '', '')
        let before = '<'.tag.attributes.' />'
        let after = ''
      else
        let before = '<'.tag.attributes.'>'
        let after  = '</'.substitute(tag,' .*','','').'>'
      endif
      if newchar == "\<C-T>"
        if type ==# "v" || type ==# "V"
          let before .= "\n\t"
        endif
        if type ==# "v"
          let after  = "\n". after
        endif
      endif
    endif
  elseif newchar ==# 'l' || newchar == '\'
    " LaTeX
    let env = input('\begin{')
    if env != ""
      let s:input = env."\<CR>"
      let env = '{' . env
      let env .= s:closematch(env)
      echo '\begin'.env
      let before = '\begin'.env
      let after  = '\end'.matchstr(env,'[^}]*').'}'
    endif
  elseif newchar ==# 'f' || newchar ==# 'F'
    let fnc = input('function: ')
    if fnc != ""
      let s:input = fnc."\<CR>"
      let before = substitute(fnc,'($','','').'('
      let after  = ')'
      if newchar ==# 'F'
        let before .= ' '
        let after = ' ' . after
      endif
    endif
  elseif newchar ==# "\<C-F>"
    let fnc = input('function: ')
    let s:input = fnc."\<CR>"
    let before = '('.fnc.' '
    let after = ')'
  elseif idx >= 0
    let spc = (idx % 3) == 1 ? " " : ""
    let idx = idx / 3 * 3
    let before = strpart(pairs,idx+1,1) . spc
    let after  = spc . strpart(pairs,idx+2,1)
  elseif newchar == "\<C-[>" || newchar == "\<C-]>"
    let before = "{\n\t"
    let after  = "\n}"
  elseif newchar !~ '\a'
    let before = newchar
    let after  = newchar
  else
    let before = ''
    let after  = ''
  endif
  let after  = substitute(after ,'\n','\n'.initspaces,'g')
  if type ==# 'V' || (a:special && type ==# "v")
    let before = substitute(before,' \+$','','')
    let after  = substitute(after ,'^ \+','','')
    if after !~ '^\n'
      let after  = initspaces.after
    endif
    if keeper !~ '\n$' && after !~ '^\n'
      let keeper .= "\n"
    elseif keeper =~ '\n$' && after =~ '^\n'
      let after = strpart(after,1)
    endif
    if keeper !~ '^\n' && before !~ '\n\s*$'
      let before .= "\n"
      if a:special
        let before .= "\t"
      endif
    elseif keeper =~ '^\n' && before =~ '\n\s*$'
      let keeper = strcharpart(keeper,1)
    endif
    if type ==# 'V' && keeper =~ '\n\s*\n$'
      let keeper = strcharpart(keeper,0,strchars(keeper) - 1)
    endif
  endif
  if type ==# 'V'
    let before = initspaces.before
  endif
  if before =~ '\n\s*\%$'
    if type ==# 'v'
      let keeper = initspaces.keeper
    endif
    let padding = matchstr(before,'\n\zs\s\+\%$')
    let before  = substitute(before,'\n\s\+\%$','\n','')
    let keeper = s:fixindent(keeper,padding)
  endif
  if type ==# 'V'
    let keeper = before.keeper.after
  elseif type =~ "^\<C-V>"
    " Really we should be iterating over the buffer
    let repl = substitute(before,'[\\~]','\\&','g').'\1'.substitute(after,'[\\~]','\\&','g')
    let repl = substitute(repl,'\n',' ','g')
    let keeper = substitute(keeper."\n",'\(.\{-\}\)\(\n\)',repl.'\n','g')
    let keeper = substitute(keeper,'\n\%$','','')
  else
    let keeper = before.extraspace.keeper.extraspace.after
  endif
  return keeper
endfunction

function! s:wrapreg(reg,char,removed,special)
  let orig = getreg(a:reg)
  let type = substitute(getregtype(a:reg),'\d\+$','','')
  let new = s:wrap(orig,a:char,type,a:removed,a:special)
  call setreg(a:reg,new,type)
endfunction
" }}}2

function! s:insert(...) " {{{2
  " Optional argument causes the result to appear on 3 lines, not 1
  let linemode = a:0 ? a:1 : 0
  let char = s:inputreplacement()
  while char == "\<CR>" || char == "\<C-S>"
    " TODO: use total count for additional blank lines
    let linemode += 1
    let char = s:inputreplacement()
  endwhile
  if char == ""
    return ""
  endif
  let cb_save = &clipboard
  set clipboard-=unnamed clipboard-=unnamedplus
  let reg_save = @@
  call setreg('"',"\r",'v')
  call s:wrapreg('"',char,"",linemode)
  " If line mode is used and the surrounding consists solely of a suffix,
  " remove the initial newline.  This fits a use case of mine but is a
  " little inconsistent.  Is there anyone that would prefer the simpler
  " behavior of just inserting the newline?
  if linemode && match(getreg('"'),'^\n\s*\zs.*') == 0
    call setreg('"',matchstr(getreg('"'),'^\n\s*\zs.*'),getregtype('"'))
  endif
  " This can be used to append a placeholder to the end
  if exists("g:surround_insert_tail")
    call setreg('"',g:surround_insert_tail,"a".getregtype('"'))
  endif
  if &ve != 'all' && col('.') >= col('$')
    if &ve == 'insert'
      let extra_cols = virtcol('.') - virtcol('$')
      if extra_cols > 0
        let [regval,regtype] = [getreg('"',1,1),getregtype('"')]
        call setreg('"',join(map(range(extra_cols),'" "'),''),'v')
        norm! ""p
        call setreg('"',regval,regtype)
      endif
    endif
    norm! ""p
  else
    norm! ""P
  endif
  if linemode
    call s:reindent()
  endif
  norm! `]
  call search('\r','bW')
  let @@ = reg_save
  let &clipboard = cb_save
  return "\<Del>"
endfunction " }}}2

function! s:reindent() " {{{2
  if exists("b:surround_indent") ? b:surround_indent : (!exists("g:surround_indent") || g:surround_indent)
    silent norm! '[=']
  endif
endfunction " }}}2

function! s:dosurround(...) " {{{2
  let scount = v:count1
  let char = (a:0 ? a:1 : s:inputtarget())
  let spc = ""
  if char =~ '^\d\+'
    let scount = scount * matchstr(char,'^\d\+')
    let char = substitute(char,'^\d\+','','')
  endif
  if char =~ '^ '
    let char = strpart(char,1)
    let spc = 1
  endif
  if char == 'a'
    let char = '>'
  endif
  if char == 'r'
    let char = ']'
  endif
  let newchar = ""
  if a:0 > 1
    let newchar = a:2
    if newchar == "\<Esc>" || newchar == "\<C-C>" || newchar == ""
      return s:beep()
    endif
  endif
  let cb_save = &clipboard
  set clipboard-=unnamed clipboard-=unnamedplus
  let append = ""
  let original = getreg('"')
  let otype = getregtype('"')
  call setreg('"',"")
  let strcount = (scount == 1 ? "" : scount)
  if char == '/'
    exe 'norm! '.strcount.'[/d'.strcount.']/'
  elseif char =~# '[[:punct:][:space:]]' && char !~# '[][(){}<>"''`]'
    exe 'norm! T'.char
    if getline('.')[col('.')-1] == char
      exe 'norm! l'
    endif
    exe 'norm! dt'.char
  else
    exe 'norm! d'.strcount.'i'.char
  endif
  let keeper = getreg('"')
  let okeeper = keeper " for reindent below
  if keeper == ""
    call setreg('"',original,otype)
    let &clipboard = cb_save
    return ""
  endif
  let oldline = getline('.')
  let oldlnum = line('.')
  if char ==# "p"
    call setreg('"','','V')
  elseif char ==# "s" || char ==# "w" || char ==# "W"
    " Do nothing
    call setreg('"','')
  elseif char =~ "[\"'`]"
    exe "norm! i \<Esc>d2i".char
    call setreg('"',substitute(getreg('"'),' ','',''))
  elseif char == '/'
    norm! "_x
    call setreg('"','/**/',"c")
    let keeper = substitute(substitute(keeper,'^/\*\s\=','',''),'\s\=\*$','','')
  elseif char =~# '[[:punct:][:space:]]' && char !~# '[][(){}<>]'
    exe 'norm! F'.char
    exe 'norm! df'.char
  else
    " One character backwards
    call search('\m.', 'bW')
    exe "norm! da".char
  endif
  let removed = getreg('"')
  let rem2 = substitute(removed,'\n.*','','')
  let oldhead = strpart(oldline,0,strlen(oldline)-strlen(rem2))
  let oldtail = strpart(oldline,  strlen(oldline)-strlen(rem2))
  let regtype = getregtype('"')
  if char =~# '[\[({<T]' || spc
    let keeper = substitute(keeper,'^\s\+','','')
    let keeper = substitute(keeper,'\s\+$','','')
  endif
  if col("']") == col("$") && virtcol('.') + 1 == virtcol('$')
    if oldhead =~# '^\s*$' && a:0 < 2
      let keeper = substitute(keeper,'\%^\n'.oldhead.'\(\s*.\{-\}\)\n\s*\%$','\1','')
    endif
    let pcmd = "p"
  else
    let pcmd = "P"
  endif
  if line('.') + 1 < oldlnum && regtype ==# "V"
    let pcmd = "p"
  endif
  call setreg('"',keeper,regtype)
  if newchar != ""
    let special = a:0 > 2 ? a:3 : 0
    call s:wrapreg('"',newchar,removed,special)
  endif
  silent exe 'norm! ""'.pcmd.'`['
  if removed =~ '\n' || okeeper =~ '\n' || getreg('"') =~ '\n'
    call s:reindent()
  endif
  if getline('.') =~ '^\s\+$' && keeper =~ '^\s*\n'
    silent norm! cc
  endif
  call setreg('"',original,otype)
  let s:lastdel = removed
  let &clipboard = cb_save
  if newchar == ""
    silent! call __repeat_set("\<Plug>Dsurround".char,scount)
  else
    silent! call __repeat_set("\<Plug>C".(a:0 > 2 && a:3 ? "S" : "s")."urround".char.newchar.s:input,scount)
  endif
endfunction " }}}2

function! s:changesurround(...) " {{{2
  let a = s:inputtarget()
  if a == ""
    return s:beep()
  endif
  let b = s:inputreplacement()
  if b == ""
    return s:beep()
  endif
  call s:dosurround(a,b,a:0 && a:1)
endfunction " }}}2

function! s:opfunc(type, ...) abort " {{{2
  if a:type ==# 'setup'
    let &opfunc = matchstr(expand('<sfile>'), '<SNR>\w\+$')
    return 'g@'
  endif
  let char = s:inputreplacement()
  if char == ""
    return s:beep()
  endif
  let reg = '"'
  let sel_save = &selection
  let &selection = "inclusive"
  let cb_save  = &clipboard
  set clipboard-=unnamed clipboard-=unnamedplus
  let reg_save = getreg(reg)
  let reg_type = getregtype(reg)
  let type = a:type
  if a:type == "char"
    silent exe 'norm! v`[o`]"'.reg.'y'
    let type = 'v'
  elseif a:type == "line"
    silent exe 'norm! `[V`]"'.reg.'y'
    let type = 'V'
  elseif a:type ==# "v" || a:type ==# "V" || a:type ==# "\<C-V>"
    let &selection = sel_save
    let ve = &virtualedit
    if !(a:0 && a:1)
      set virtualedit=
    endif
    silent exe 'norm! gv"'.reg.'y'
    let &virtualedit = ve
  elseif a:type =~ '^\d\+$'
    let type = 'v'
    silent exe 'norm! ^v'.a:type.'$h"'.reg.'y'
    if mode() ==# 'v'
      norm! v
      return s:beep()
    endif
  else
    let &selection = sel_save
    let &clipboard = cb_save
    return s:beep()
  endif
  let keeper = getreg(reg)
  if type ==# "v" && a:type !=# "v"
    let append = matchstr(keeper,'\_s\@<!\s*$')
    let keeper = substitute(keeper,'\_s\@<!\s*$','','')
  endif
  call setreg(reg,keeper,type)
  call s:wrapreg(reg,char,"",a:0 && a:1)
  if type ==# "v" && a:type !=# "v" && append != ""
    call setreg(reg,append,"ac")
  endif
  silent exe 'norm! gv'.(reg == '"' ? '' : '"' . reg).'p`['
  if type ==# 'V' || (getreg(reg) =~ '\n' && type ==# 'v')
    call s:reindent()
  endif
  call setreg(reg,reg_save,reg_type)
  let &selection = sel_save
  let &clipboard = cb_save
  if a:type =~ '^\d\+$'
    silent! call __repeat_set("\<Plug>Y".(a:0 && a:1 ? "S" : "s")."surround".char.s:input,a:type)
  else
    silent! call __repeat_set("\<Plug>SurroundRepeat".char.s:input)
  endif
endfunction

function! s:opfunc2(...) abort
  if !a:0 || a:1 ==# 'setup'
    let &opfunc = matchstr(expand('<sfile>'), '<SNR>\w\+$')
    return 'g@'
  endif
  call s:opfunc(a:1, 1)
endfunction " }}}2

function! s:closematch(str) " {{{2
  " Close an open (, {, [, or < on the command line.
  let tail = matchstr(a:str,'.[^\[\](){}<>]*$')
  if tail =~ '^\[.\+'
    return "]"
  elseif tail =~ '^(.\+'
    return ")"
  elseif tail =~ '^{.\+'
    return "}"
  elseif tail =~ '^<.+'
    return ">"
  else
    return ""
  endif
endfunction " }}}2

nnoremap <silent> <Plug>SurroundRepeat .
nnoremap <silent> <Plug>Dsurround  :<C-U>call <SID>dosurround(<SID>inputtarget())<CR>
nnoremap <silent> <Plug>Csurround  :<C-U>call <SID>changesurround()<CR>
nnoremap <silent> <Plug>CSurround  :<C-U>call <SID>changesurround(1)<CR>
nnoremap <expr>   <Plug>Yssurround '^'.v:count1.<SID>opfunc('setup').'g_'
nnoremap <expr>   <Plug>YSsurround <SID>opfunc2('setup').'_'
nnoremap <expr>   <Plug>Ysurround  <SID>opfunc('setup')
nnoremap <expr>   <Plug>YSurround  <SID>opfunc2('setup')
vnoremap <silent> <Plug>VSurround  :<C-U>call <SID>opfunc(visualmode(),visualmode() ==# 'V' ? 1 : 0)<CR>
vnoremap <silent> <Plug>VgSurround :<C-U>call <SID>opfunc(visualmode(),visualmode() ==# 'V' ? 0 : 1)<CR>
inoremap <silent> <Plug>Isurround  <C-R>=<SID>insert()<CR>
inoremap <silent> <Plug>ISurround  <C-R>=<SID>insert(1)<CR>

if !exists("g:surround_no_mappings") || ! g:surround_no_mappings
  nmap ds  <Plug>Dsurround
  nmap cs  <Plug>Csurround
  nmap cS  <Plug>CSurround
  nmap ys  <Plug>Ysurround
  nmap yS  <Plug>YSurround
  nmap yss <Plug>Yssurround
  nmap ySs <Plug>YSsurround
  nmap ySS <Plug>YSsurround
  xmap S   <Plug>VSurround
  xmap gS  <Plug>VgSurround
  if !exists("g:surround_no_insert_mappings") || ! g:surround_no_insert_mappings
    if !hasmapto("<Plug>Isurround","i") && "" == mapcheck("<C-S>","i")
      imap    <C-S> <Plug>Isurround
    endif
    imap      <C-G>s <Plug>Isurround
    imap      <C-G>S <Plug>ISurround
  endif
endif

" }}}1



" commentary.vim - Comment stuff out {{{1
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.3
" GetLatestVimScripts: 3695 1 :AutoInstall: commentary.vim

if exists("g:loaded_commentary") || v:version < 700
  finish
endif
let g:loaded_commentary = 1

function! s:surroundings() abort
  return split(get(b:, 'commentary_format', substitute(substitute(substitute(
        \ &commentstring, '^$', '%s', ''), '\S\zs%s',' %s', '') ,'%s\ze\S', '%s ', '')), '%s', 1)
endfunction

function! s:strip_white_space(l,r,line) abort
  let [l, r] = [a:l, a:r]
  if l[-1:] ==# ' ' && stridx(a:line,l) == -1 && stridx(a:line,l[0:-2]) == 0
    let l = l[:-2]
  endif
  if r[0] ==# ' ' && a:line[-strlen(r):] != r && a:line[1-strlen(r):] == r[1:]
    let r = r[1:]
  endif
  return [l, r]
endfunction

function! s:go(...) abort
  if !a:0
    let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
    return 'g@'
  elseif a:0 > 1
    let [lnum1, lnum2] = [a:1, a:2]
  else
    let [lnum1, lnum2] = [line("'["), line("']")]
  endif

  let [l, r] = s:surroundings()
  let uncomment = 2
  for lnum in range(lnum1,lnum2)
    let line = matchstr(getline(lnum),'\S.*\s\@<!')
    let [l, r] = s:strip_white_space(l,r,line)
    if len(line) && (stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
      let uncomment = 0
    endif
  endfor

  if get(b:, 'commentary_startofline')
    let indent = '^'
  else
    let indent = '^\s*'
  endif

  for lnum in range(lnum1,lnum2)
    let line = getline(lnum)
    if strlen(r) > 2 && l.r !~# '\\'
      let line = substitute(line,
            \'\M' . substitute(l, '\ze\S\s*$', '\\zs\\d\\*\\ze', '') . '\|' . substitute(r, '\S\zs', '\\zs\\d\\*\\ze', ''),
            \'\=substitute(submatch(0)+1-uncomment,"^0$\\|^-\\d*$","","")','g')
    endif
    if uncomment
      let line = substitute(line,'\S.*\s\@<!','\=submatch(0)[strlen(l):-strlen(r)-1]','')
    else
      let line = substitute(line,'^\%('.matchstr(getline(lnum1),indent).'\|\s*\)\zs.*\S\@<=','\=l.submatch(0).r','')
    endif
    call setline(lnum,line)
  endfor
  let modelines = &modelines
  try
    set modelines=0
    silent doautocmd User CommentaryPost
  finally
    let &modelines = modelines
  endtry
  return ''
endfunction

function! s:textobject(inner) abort
  let [l, r] = s:surroundings()
  let lnums = [line('.')+1, line('.')-2]
  for [index, dir, bound, line] in [[0, -1, 1, ''], [1, 1, line('$'), '']]
    while lnums[index] != bound && line ==# '' || !(stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
      let lnums[index] += dir
      let line = matchstr(getline(lnums[index]+dir),'\S.*\s\@<!')
      let [l, r] = s:strip_white_space(l,r,line)
    endwhile
  endfor
  while (a:inner || lnums[1] != line('$')) && empty(getline(lnums[0]))
    let lnums[0] += 1
  endwhile
  while a:inner && empty(getline(lnums[1]))
    let lnums[1] -= 1
  endwhile
  if lnums[0] <= lnums[1]
    execute 'normal! 'lnums[0].'GV'.lnums[1].'G'
  endif
endfunction

command! -range -bar Commentary call s:go(<line1>,<line2>)
xnoremap <expr>   <Plug>Commentary     <SID>go()
nnoremap <expr>   <Plug>Commentary     <SID>go()
nnoremap <expr>   <Plug>CommentaryLine <SID>go() . '_'
onoremap <silent> <Plug>Commentary        :<C-U>call <SID>textobject(get(v:, 'operator', '') ==# 'c')<CR>
nnoremap <silent> <Plug>ChangeCommentary c:<C-U>call <SID>textobject(1)<CR>
nmap <silent> <Plug>CommentaryUndo :echoerr "Change your <Plug>CommentaryUndo map to <Plug>Commentary<Plug>Commentary"<CR>

if !hasmapto('<Plug>Commentary') || maparg('gc','n') ==# ''
  xmap gc  <Plug>Commentary
  nmap gc  <Plug>Commentary
  omap gc  <Plug>Commentary
  nmap gcc <Plug>CommentaryLine
  if maparg('c','n') ==# '' && !exists('v:operator')
    nmap cgc <Plug>ChangeCommentary
  endif
  nmap gcu <Plug>Commentary<Plug>Commentary
endif

" }}}1



" auto-pairs - Insert or delete brackets, parens, quotes in pairs. {{{1
" Maintainer:	JiangMiao <jiangfriend@gmail.com>
" Contributor: camthompson
" Last Change:  2019-02-02
" Version: 2.0.0
" Homepage: http://www.vim.org/scripts/script.php?script_id=3599
" Repository: https://github.com/jiangmiao/auto-pairs
" License: MIT

if exists('g:AutoPairsLoaded') || &cp
  finish
end
let g:AutoPairsLoaded = 1

if !exists('g:AutoPairs')
  let g:AutoPairs = {'(':')', '[':']', '{':'}',"'":"'",'"':'"', '```':'```', '"""':'"""', "'''":"'''", "`":"`"}
end

" default pairs base on filetype
func! AutoPairsDefaultPairs()
  if exists('b:autopairs_defaultpairs')
    return b:autopairs_defaultpairs
  end
  let r = copy(g:AutoPairs)
  let allPairs = {
        \ 'vim': {'\v^\s*\zs"': ''},
        \ 'rust': {'\w\zs<': '>', '&\zs''': ''},
        \ 'php': {'<?': '?>//k]', '<?php': '?>//k]'}
        \ }
  for [filetype, pairs] in items(allPairs)
    if &filetype == filetype
      for [open, close] in items(pairs)
        let r[open] = close
      endfor
    end
  endfor
  let b:autopairs_defaultpairs = r
  return r
endf

if !exists('g:AutoPairsMapBS')
  let g:AutoPairsMapBS = 1
end

" Map <C-h> as the same BS
if !exists('g:AutoPairsMapCh')
  let g:AutoPairsMapCh = 1
end

if !exists('g:AutoPairsMapCR')
  let g:AutoPairsMapCR = 1
end

if !exists('g:AutoPairsWildClosedPair')
  let g:AutoPairsWildClosedPair = ''
end

if !exists('g:AutoPairsMapSpace')
  let g:AutoPairsMapSpace = 1
end

if !exists('g:AutoPairsCenterLine')
  let g:AutoPairsCenterLine = 1
end

if !exists('g:AutoPairsShortcutToggle')
  let g:AutoPairsShortcutToggle = '<M-p>'
end

if !exists('g:AutoPairsShortcutFastWrap')
  let g:AutoPairsShortcutFastWrap = '<M-e>'
end

if !exists('g:AutoPairsMoveCharacter')
  let g:AutoPairsMoveCharacter = "()[]{}\"'"
end

if !exists('g:AutoPairsShortcutJump')
  let g:AutoPairsShortcutJump = '<M-n>'
endif

" Fly mode will for closed pair to jump to closed pair instead of insert.
" also support AutoPairsBackInsert to insert pairs where jumped.
if !exists('g:AutoPairsFlyMode')
  let g:AutoPairsFlyMode = 0
endif

" When skipping the closed pair, look at the current and
" next line as well.
if !exists('g:AutoPairsMultilineClose')
  let g:AutoPairsMultilineClose = 1
endif

" Work with Fly Mode, insert pair where jumped
if !exists('g:AutoPairsShortcutBackInsert')
  let g:AutoPairsShortcutBackInsert = '<M-b>'
endif

if !exists('g:AutoPairsSmartQuotes')
  let g:AutoPairsSmartQuotes = 1
endif

" 7.4.849 support <C-G>U to avoid breaking '.'
" Issue talk: https://github.com/jiangmiao/auto-pairs/issues/3
" Vim note: https://github.com/vim/vim/releases/tag/v7.4.849
if v:version > 704 || v:version == 704 && has("patch849")
  let s:Go = "\<C-G>U"
else
  let s:Go = ""
endif

let s:Left = s:Go."\<LEFT>"
let s:Right = s:Go."\<RIGHT>"




" unicode len
func! s:ulen(s)
  return len(split(a:s, '\zs'))
endf

func! s:left(s)
  return repeat(s:Left, s:ulen(a:s))
endf

func! s:right(s)
  return repeat(s:Right, s:ulen(a:s))
endf

func! s:delete(s)
  return repeat("\<DEL>", s:ulen(a:s))
endf

func! s:backspace(s)
  return repeat("\<BS>", s:ulen(a:s))
endf

func! s:getline()
  let line = getline('.')
  let pos = col('.') - 1
  let before = strpart(line, 0, pos)
  let after = strpart(line, pos)
  let afterline = after
  if g:AutoPairsMultilineClose
    let n = line('$')
    let i = line('.')+1
    while i <= n
      let line = getline(i)
      let after = after.' '.line
      if !(line =~ '\v^\s*$')
        break
      end
      let i = i+1
    endwhile
  end
  return [before, after, afterline]
endf

" split text to two part
" returns [orig, text_before_open, open]
func! s:matchend(text, open)
    let m = matchstr(a:text, '\V'.a:open.'\v$')
    if m == ""
      return []
    end
    return [a:text, strpart(a:text, 0, len(a:text)-len(m)), m]
endf

" returns [orig, close, text_after_close]
func! s:matchbegin(text, close)
    let m = matchstr(a:text, '^\V'.a:close)
    if m == ""
      return []
    end
    return [a:text, m, strpart(a:text, len(m), len(a:text)-len(m))]
endf

" add or delete pairs base on g:AutoPairs
" AutoPairsDefine(addPairs:dict[, removeOpenPairList:list])
"
" eg:
"   au FileType html let b:AutoPairs = AutoPairsDefine({'<!--' : '-->'}, ['{'])
"   add <!-- --> pair and remove '{' for html file
func! AutoPairsDefine(pairs, ...)
  let r = AutoPairsDefaultPairs()
  if a:0 > 0
    for open in a:1
      unlet r[open]
    endfor
  end
  for [open, close] in items(a:pairs)
    let r[open] = close
  endfor
  return r
endf

func! AutoPairsInsert(key)
  if !b:autopairs_enabled
    return a:key
  end

  let b:autopairs_saved_pair = [a:key, getpos('.')]

  let [before, after, afterline] = s:getline()

  " Ignore auto close if prev character is \
  if before[-1:-1] == '\'
    return a:key
  end

  " check open pairs
  for [open, close, opt] in b:AutoPairsList
    let ms = s:matchend(before.a:key, open)
    let m = matchstr(afterline, '^\v\s*\zs\V'.close)
    if len(ms) > 0
      " process the open pair

      " remove inserted pair
      " eg: if the pairs include < > and  <!-- -->
      " when <!-- is detected the inserted pair < > should be clean up
      let target = ms[1]
      let openPair = ms[2]
      if len(openPair) == 1 && m == openPair
        break
      end
      let bs = ''
      let del = ''
      while len(before) > len(target)
        let found = 0
        " delete pair
        for [o, c, opt] in b:AutoPairsList
          let os = s:matchend(before, o)
          if len(os) && len(os[1]) < len(target)
            " any text before openPair should not be deleted
            continue
          end
          let cs = s:matchbegin(afterline, c)
          if len(os) && len(cs)
            let found = 1
            let before = os[1]
            let afterline = cs[2]
            let bs = bs.s:backspace(os[2])
            let del = del.s:delete(cs[1])
            break
          end
        endfor
        if !found
          " delete charactor
          let ms = s:matchend(before, '\v.')
          if len(ms)
            let before = ms[1]
            let bs = bs.s:backspace(ms[2])
          end
        end
      endwhile
      return bs.del.openPair.close.s:left(close)
    end
  endfor

  " check close pairs
  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end
    if a:key == g:AutoPairsWildClosedPair || opt['mapclose'] && opt['key'] == a:key
      " the close pair is in the same line
      let m = matchstr(afterline, '^\v\s*\V'.close)
      if m != ''
        if before =~ '\V'.open.'\v\s*$' && m[0] =~ '\v\s'
          " remove the space we inserted if the text in pairs is blank
          return "\<DEL>".s:right(m[1:])
        else
          return s:right(m)
        end
      end
      let m = matchstr(after, '^\v\s*\zs\V'.close)
      if m != ''
        if a:key == g:AutoPairsWildClosedPair || opt['multiline']
          if b:autopairs_return_pos == line('.') && getline('.') =~ '\v^\s*$'
            normal! ddk$
          end
          call search(m, 'We')
          return "\<Right>"
        else
          break
        end
      end
    end
  endfor


  " Fly Mode, and the key is closed-pairs, search closed-pair and jump
  if g:AutoPairsFlyMode &&  a:key =~ '\v[\}\]\)]'
    if search(a:key, 'We')
      return "\<Right>"
    endif
  endif

  return a:key
endf

func! AutoPairsDelete()
  if !b:autopairs_enabled
    return "\<BS>"
  end

  let [before, after, ig] = s:getline()
  for [open, close, opt] in b:AutoPairsList
    let b = matchstr(before, '\V'.open.'\v\s?$')
    let a = matchstr(after, '^\v\s*\V'.close)
    if b != '' && a != ''
      if b[-1:-1] == ' '
        if a[0] == ' '
          return "\<BS>\<DELETE>"
        else
          return "\<BS>"
        end
      end
      return s:backspace(b).s:delete(a)
    end
  endfor

  return "\<BS>"
  " delete the pair foo[]| <BS> to foo
  for [open, close, opt] in b:AutoPairsList
    let m = s:matchend(before, '\V'.open.'\v\s*'.'\V'.close.'\v$')
    if len(m) > 0
      return s:backspace(m[2])
    end
  endfor
  return "\<BS>"
endf


" Fast wrap the word in brackets
func! AutoPairsFastWrap()
  let c = @"
  normal! x
  let [before, after, ig] = s:getline()
  if after[0] =~ '\v[\{\[\(\<]'
    normal! %
    normal! p
  else
    for [open, close, opt] in b:AutoPairsList
      if close == ''
        continue
      end
      if after =~ '^\s*\V'.open
        call search(close, 'We')
        normal! p
        let @" = c
        return ""
      end
    endfor
    if after[1:1] =~ '\v\w'
      normal! e
      normal! p
    else
      normal! p
    end
  end
  let @" = c
  return ""
endf

func! AutoPairsJump()
  call search('["\]'')}]','W')
endf

func! AutoPairsMoveCharacter(key)
  let c = getline(".")[col(".")-1]
  let escaped_key = substitute(a:key, "'", "''", 'g')
  return "\<DEL>\<ESC>:call search("."'".escaped_key."'".")\<CR>a".c."\<LEFT>"
endf

func! AutoPairsBackInsert()
  let pair = b:autopairs_saved_pair[0]
  let pos  = b:autopairs_saved_pair[1]
  call setpos('.', pos)
  return pair
endf

func! AutoPairsReturn()
  if b:autopairs_enabled == 0
    return ''
  end
  let b:autopairs_return_pos = 0
  let before = getline(line('.')-1)
  let [ig, ig, afterline] = s:getline()
  let cmd = ''
  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end

    if before =~ '\V'.open.'\v\s*$' && afterline =~ '^\s*\V'.close
      let b:autopairs_return_pos = line('.')
      if g:AutoPairsCenterLine && winline() * 3 >= winheight(0) * 2
        " Recenter before adding new line to avoid replacing line content
        let cmd = "zz"
      end

      " If equalprg has been set, then avoid call =
      " https://github.com/jiangmiao/auto-pairs/issues/24
      if &equalprg != ''
        return "\<ESC>".cmd."O"
      endif

      " conflict with javascript and coffee
      " javascript   need   indent new line
      " coffeescript forbid indent new line
      if &filetype == 'coffeescript' || &filetype == 'coffee'
        return "\<ESC>".cmd."k==o"
      else
        return "\<ESC>".cmd."=ko"
      endif
    end
  endfor
  return ''
endf

func! AutoPairsSpace()
  if !b:autopairs_enabled
    return "\<SPACE>"
  end

  let [before, after, ig] = s:getline()

  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end
    if before =~ '\V'.open.'\v$' && after =~ '^\V'.close
      if close =~ '\v^[''"`]$'
        return "\<SPACE>"
      else
        return "\<SPACE>\<SPACE>".s:Left
      end
    end
  endfor
  return "\<SPACE>"
endf

func! AutoPairsMap(key)
  " | is special key which separate map command from text
  let key = a:key
  if key == '|'
    let key = '<BAR>'
  end
  let escaped_key = substitute(key, "'", "''", 'g')
  " use expr will cause search() doesn't work
  execute 'inoremap <buffer> <silent> '.key." <C-R>=AutoPairsInsert('".escaped_key."')<CR>"
endf

func! AutoPairsToggle()
  if b:autopairs_enabled
    let b:autopairs_enabled = 0
    echo 'AutoPairs Disabled.'
  else
    let b:autopairs_enabled = 1
    echo 'AutoPairs Enabled.'
  end
  return ''
endf

func! s:sortByLength(i1, i2)
  return len(a:i2[0])-len(a:i1[0])
endf

func! AutoPairsInit()
  let b:autopairs_loaded  = 1
  if !exists('b:autopairs_enabled')
    let b:autopairs_enabled = 1
  end

  if !exists('b:AutoPairs')
    let b:AutoPairs = AutoPairsDefaultPairs()
  end

  if !exists('b:AutoPairsMoveCharacter')
    let b:AutoPairsMoveCharacter = g:AutoPairsMoveCharacter
  end

  let b:autopairs_return_pos = 0
  let b:autopairs_saved_pair = [0, 0]
  let b:AutoPairsList = []

  " buffer level map pairs keys
  " n - do not map the first charactor of closed pair to close key
  " m - close key jumps through multi line
  " s - close key jumps only in the same line
  for [open, close] in items(b:AutoPairs)
    let o = open[-1:-1]
    let c = close[0]
    let opt = {'mapclose': 1, 'multiline':1}
    let opt['key'] = c
    if o == c
      let opt['multiline'] = 0
    end
    let m = matchlist(close, '\v(.*)//(.*)$')
    if len(m) > 0
      if m[2] =~ 'n'
        let opt['mapclose'] = 0
      end
      if m[2] =~ 'm'
        let opt['multiline'] = 1
      end
      if m[2] =~ 's'
        let opt['multiline'] = 0
      end
      let ks = matchlist(m[2], '\vk(.)')
      if len(ks) > 0
        let opt['key'] = ks[1]
        let c = opt['key']
      end
      let close = m[1]
    end
    call AutoPairsMap(o)
    if o != c && c != '' && opt['mapclose']
      call AutoPairsMap(c)
    end
    let b:AutoPairsList += [[open, close, opt]]
  endfor

  " sort pairs by length, longer pair should have higher priority
  let b:AutoPairsList = sort(b:AutoPairsList, "s:sortByLength")

  for item in b:AutoPairsList
    let [open, close, opt] = item
    if open == "'" && open == close
      let item[0] = '\v(^|\W)\zs'''
    end
  endfor


  for key in split(b:AutoPairsMoveCharacter, '\s*')
    let escaped_key = substitute(key, "'", "''", 'g')
    execute 'inoremap <silent> <buffer> <M-'.key."> <C-R>=AutoPairsMoveCharacter('".escaped_key."')<CR>"
  endfor

  " Still use <buffer> level mapping for <BS> <SPACE>
  if g:AutoPairsMapBS
    " Use <C-R> instead of <expr> for issue #14 sometimes press BS output strange words
    execute 'inoremap <buffer> <silent> <BS> <C-R>=AutoPairsDelete()<CR>'
  end

  if g:AutoPairsMapCh
    execute 'inoremap <buffer> <silent> <C-h> <C-R>=AutoPairsDelete()<CR>'
  endif

  if g:AutoPairsMapSpace
    " Try to respect abbreviations on a <SPACE>
    let do_abbrev = ""
    if v:version == 703 && has("patch489") || v:version > 703
      let do_abbrev = "<C-]>"
    endif
    execute 'inoremap <buffer> <silent> <SPACE> '.do_abbrev.'<C-R>=AutoPairsSpace()<CR>'
  end

  if g:AutoPairsShortcutFastWrap != ''
    execute 'inoremap <buffer> <silent> '.g:AutoPairsShortcutFastWrap.' <C-R>=AutoPairsFastWrap()<CR>'
  end

  if g:AutoPairsShortcutBackInsert != ''
    execute 'inoremap <buffer> <silent> '.g:AutoPairsShortcutBackInsert.' <C-R>=AutoPairsBackInsert()<CR>'
  end

  if g:AutoPairsShortcutToggle != ''
    " use <expr> to ensure showing the status when toggle
    execute 'inoremap <buffer> <silent> <expr> '.g:AutoPairsShortcutToggle.' AutoPairsToggle()'
    execute 'noremap <buffer> <silent> '.g:AutoPairsShortcutToggle.' :call AutoPairsToggle()<CR>'
  end

  if g:AutoPairsShortcutJump != ''
    execute 'inoremap <buffer> <silent> ' . g:AutoPairsShortcutJump. ' <ESC>:call AutoPairsJump()<CR>a'
    execute 'noremap <buffer> <silent> ' . g:AutoPairsShortcutJump. ' :call AutoPairsJump()<CR>'
  end

  if &keymap != ''
    let l:imsearch = &imsearch
    let l:iminsert = &iminsert
    let l:imdisable = &imdisable
    execute 'setlocal keymap=' . &keymap
    execute 'setlocal imsearch=' . l:imsearch
    execute 'setlocal iminsert=' . l:iminsert
    if l:imdisable
      execute 'setlocal imdisable'
    else
      execute 'setlocal noimdisable'
    end
  end

endf

func! s:ExpandMap(map)
  let map = a:map
  let map = substitute(map, '\(<Plug>\w\+\)', '\=maparg(submatch(1), "i")', 'g')
  let map = substitute(map, '\(<Plug>([^)]*)\)', '\=maparg(submatch(1), "i")', 'g')
  return map
endf

func! AutoPairsTryInit()
  if exists('b:autopairs_loaded')
    return
  end

  " for auto-pairs starts with 'a', so the priority is higher than supertab and vim-endwise
  "
  " vim-endwise doesn't support <Plug>AutoPairsReturn
  " when use <Plug>AutoPairsReturn will cause <Plug> isn't expanded
  "
  " supertab doesn't support <SID>AutoPairsReturn
  " when use <SID>AutoPairsReturn  will cause Duplicated <CR>
  "
  " and when load after vim-endwise will cause unexpected endwise inserted.
  " so always load AutoPairs at last

  " Buffer level keys mapping
  " comptible with other plugin
  if g:AutoPairsMapCR
    if v:version == 703 && has('patch32') || v:version > 703
      " VIM 7.3 supports advancer maparg which could get <expr> info
      " then auto-pairs could remap <CR> in any case.
      let info = maparg('<CR>', 'i', 0, 1)
      if empty(info)
        let old_cr = '<CR>'
        let is_expr = 0
      else
        let old_cr = info['rhs']
        let old_cr = s:ExpandMap(old_cr)
        let old_cr = substitute(old_cr, '<SID>', '<SNR>' . info['sid'] . '_', 'g')
        let is_expr = info['expr']
        let wrapper_name = '<SID>AutoPairsOldCRWrapper73'
      endif
    else
      " VIM version less than 7.3
      " the mapping's <expr> info is lost, so guess it is expr or not, it's
      " not accurate.
      let old_cr = maparg('<CR>', 'i')
      if old_cr == ''
        let old_cr = '<CR>'
        let is_expr = 0
      else
        let old_cr = s:ExpandMap(old_cr)
        " old_cr contain (, I guess the old cr is in expr mode
        let is_expr = old_cr =~ '\V(' && toupper(old_cr) !~ '\V<C-R>'

        " The old_cr start with " it must be in expr mode
        let is_expr = is_expr || old_cr =~ '\v^"'
        let wrapper_name = '<SID>AutoPairsOldCRWrapper'
      end
    end

    if old_cr !~ 'AutoPairsReturn'
      if is_expr
        " remap <expr> to `name` to avoid mix expr and non-expr mode
        execute 'inoremap <buffer> <expr> <script> '. wrapper_name . ' ' . old_cr
        let old_cr = wrapper_name
      end
      " Always silent mapping
      execute 'inoremap <script> <buffer> <silent> <CR> '.old_cr.'<SID>AutoPairsReturn'
    end
  endif
  call AutoPairsInit()
endf

" Always silent the command
inoremap <silent> <SID>AutoPairsReturn <C-R>=AutoPairsReturn()<CR>
imap <script> <Plug>AutoPairsReturn <SID>AutoPairsReturn


au BufEnter * :call AutoPairsTryInit()

" }}}1



" vim-cool - Disable hlsearch when you are done searching. {{{1
" Maintainer:	romainl <romainlafourcade@gmail.com>
" Version:	0.0.2
" License:	MIT License
" Location:	plugin/cool.vim
" Website:	https://github.com/romainl/vim-cool

if exists("g:loaded_cool") || v:version < 704 || &compatible
    finish
endif
let g:loaded_cool = 1

let s:save_cpo = &cpo
set cpo&vim

augroup Cool
    autocmd!
augroup END

if exists('##OptionSet')
    if !exists('*execute')
        autocmd Cool OptionSet highlight let <SID>saveh = &highlight
    endif
    " toggle coolness when hlsearch is toggled
    autocmd Cool OptionSet hlsearch call <SID>PlayItCool(v:option_old, v:option_new)
endif

function! s:StartHL()
    if !v:hlsearch || mode() isnot 'n'
        return
    endif
    let [pos, rpos] = [winsaveview(), getpos('.')]
    silent! exe "keepjumps go".(line2byte('.')+col('.')-(v:searchforward ? 2 : 0))
    try
        silent keepjumps norm! n
        if getpos('.') != rpos
            throw 0
        endif
    catch /^\%(0$\|Vim\%(\w\|:Interrupt$\)\@!\)/
        call <SID>StopHL()
        return
    finally
        call winrestview(pos)
    endtry
    if !get(g:,'CoolTotalMatches') || !exists('*reltimestr')
        return
    endif
    exe "silent! norm! :let g:cool_char=nr2char(screenchar(screenrow(),1))\<cr>"
    let cool_char = remove(g:,'cool_char')
    if cool_char !~ '[/?]'
        return
    endif
    let [f, ws, now, noOf] = [0, &wrapscan, reltime(), [0,0]]
    set nowrapscan
    try
        while f < 2
            if reltimestr(reltime(now))[:-6] =~ '[1-9]'
                " time >= 100ms
                return
            endif
            let noOf[v:searchforward ? f : !f] += 1
            try
                silent exe "keepjumps norm! ".(f ? 'n' : 'N')
            catch /^Vim[^)]\+):E38[45]\D/
                call setpos('.',rpos)
                let f += 1
            endtry
        endwhile
    finally
        call winrestview(pos)
        let &wrapscan = ws
    endtry
    redraw|echo cool_char.@/ 'match' noOf[0] 'of' noOf[0] + noOf[1] - 1
endfunction

function! s:StopHL()
    if !v:hlsearch || mode() isnot 'n'
        return
    else
        silent call feedkeys("\<Plug>(StopHL)", 'm')
    endif
endfunction

if !exists('*execute')
    let s:saveh = &highlight
    " toggle highlighting, a workaround for :nohlsearch in autocmds
    function! s:AuNohlsearch()
        noautocmd set highlight+=l:-
        autocmd Cool Insertleave *
                    \ noautocmd let &highlight = s:saveh | autocmd! Cool InsertLeave *
        return ''
    endfunction
endif

function! s:PlayItCool(old, new)
    if a:old == 0 && a:new == 1
        " nohls --> hls
        "   set up coolness
        noremap <silent> <Plug>(StopHL) :<C-U>nohlsearch<cr>
        if !exists('*execute')
            noremap! <expr> <Plug>(StopHL) <SID>AuNohlsearch()
        else
            noremap! <expr> <Plug>(StopHL) execute('nohlsearch')[-1]
        endif

        autocmd Cool CursorMoved * call <SID>StartHL()
        autocmd Cool InsertEnter * call <SID>StopHL()
    elseif a:old == 1 && a:new == 0
        " hls --> nohls
        "   tear down coolness
        nunmap <Plug>(StopHL)
        unmap! <expr> <Plug>(StopHL)

        autocmd! Cool CursorMoved
        autocmd! Cool InsertEnter
    else
        " nohls --> nohls
        "   do nothing
        return
    endif
endfunction

" play it cool
call <SID>PlayItCool(0, &hlsearch)

let &cpo = s:save_cpo

" }}}1



" vim-buftabline - Show buffer list in the tabline {{{1
" Tweaked a bit to make it work in one file
" Vim global plugin for rendering the buffer list in the tabline
" Licence:     The MIT License (MIT)
" Commit:      $Format:%H$
" {{{ Copyright (c) 2015 Aristotle Pagaltzis <pagaltzis@gmx.de>
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

if v:version < 700
	echoerr printf('Vim 7 is required for buftabline (this is only %d.%d)',v:version/100,v:version%100)
	finish
endif

scriptencoding utf-8

hi default link BufTabLineCurrent TabLineSel
hi default link BufTabLineActive  PmenuSel
hi default link BufTabLineHidden  TabLine
hi default link BufTabLineFill    TabLineFill

let g:buftabline_numbers    = get(g:, 'buftabline_numbers',    0)
let g:buftabline_indicators = get(g:, 'buftabline_indicators', 0)
let g:buftabline_separators = get(g:, 'buftabline_separators', 0)
let g:buftabline_show       = get(g:, 'buftabline_show',       2)
let g:buftabline_plug_max   = get(g:, 'buftabline_plug_max',  10)

function! s:buftabline_user_buffers() " help buffers are always unlisted, but quickfix buffers are not
	return filter(range(1,bufnr('$')),'buflisted(v:val) && "quickfix" !=? getbufvar(v:val, "&buftype")')
endfunction

function! s:switch_buffer(bufnum, clicks, button, mod)
	execute 'buffer' a:bufnum
endfunction

function s:SID()
	return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfunction

let s:dirsep = fnamemodify(getcwd(),':p')[-1:]
let s:centerbuf = winbufnr(0)
let s:tablineat = has('tablineat')
let s:sid = s:SID() | delfunction s:SID
function! __buftabline_render()
	let show_num = g:buftabline_numbers == 1
	let show_ord = g:buftabline_numbers == 2
	let show_mod = g:buftabline_indicators
	let lpad     = g:buftabline_separators ? nr2char(0x23B8) : ' '

	let bufnums = s:buftabline_user_buffers()
	let centerbuf = s:centerbuf " prevent tabline jumping around when non-user buffer current (e.g. help)

	" pick up data on all the buffers
	let tabs = []
	let path_tabs = []
	let tabs_per_tail = {}
	let currentbuf = winbufnr(0)
	let screen_num = 0
	for bufnum in bufnums
		let screen_num = show_num ? bufnum : show_ord ? screen_num + 1 : ''
		let tab = { 'num': bufnum }
		let tab.hilite = currentbuf == bufnum ? 'Current' : bufwinnr(bufnum) > 0 ? 'Active' : 'Hidden'
		if currentbuf == bufnum | let [centerbuf, s:centerbuf] = [bufnum, bufnum] | endif
		let bufpath = bufname(bufnum)
		if strlen(bufpath)
			let tab.path = fnamemodify(bufpath, ':p:~:.')
			let tab.sep = strridx(tab.path, s:dirsep, strlen(tab.path) - 2) " keep trailing dirsep
			let tab.label = tab.path[tab.sep + 1:]
			let pre = ( show_mod && getbufvar(bufnum, '&mod') ? '+' : '' ) . screen_num
			let tab.pre = strlen(pre) ? pre . ' ' : ''
			let tabs_per_tail[tab.label] = get(tabs_per_tail, tab.label, 0) + 1
			let path_tabs += [tab]
		elseif -1 < index(['nofile','acwrite'], getbufvar(bufnum, '&buftype')) " scratch buffer
			let tab.label = ( show_mod ? '!' . screen_num : screen_num ? screen_num . ' !' : '!' )
		else " unnamed file
			let tab.label = ( show_mod && getbufvar(bufnum, '&mod') ? '+' : '' )
			\             . ( screen_num ? screen_num : '*' )
		endif
		let tabs += [tab]
	endfor

	" disambiguate same-basename files by adding trailing path segments
	while len(filter(tabs_per_tail, 'v:val > 1'))
		let [ambiguous, tabs_per_tail] = [tabs_per_tail, {}]
		for tab in path_tabs
			if -1 < tab.sep && has_key(ambiguous, tab.label)
				let tab.sep = strridx(tab.path, s:dirsep, tab.sep - 1)
				let tab.label = tab.path[tab.sep + 1:]
			endif
			let tabs_per_tail[tab.label] = get(tabs_per_tail, tab.label, 0) + 1
		endfor
	endwhile

	" now keep the current buffer center-screen as much as possible:

	" 1. setup
	let lft = { 'lasttab':  0, 'cut':  '.', 'indicator': '<', 'width': 0, 'half': &columns / 2 }
	let rgt = { 'lasttab': -1, 'cut': '.$', 'indicator': '>', 'width': 0, 'half': &columns - lft.half }

	" 2. sum the string lengths for the left and right halves
	let currentside = lft
	for tab in tabs
		let tab.label = lpad . get(tab, 'pre', '') . tab.label . ' '
		let tab.width = strwidth(strtrans(tab.label))
		if centerbuf == tab.num
			let halfwidth = tab.width / 2
			let lft.width += halfwidth
			let rgt.width += tab.width - halfwidth
			let currentside = rgt
			continue
		endif
		let currentside.width += tab.width
	endfor
	if currentside is lft " centered buffer not seen?
		" then blame any overflow on the right side, to protect the left
		let [lft.width, rgt.width] = [0, lft.width]
	endif

	" 3. toss away tabs and pieces until all fits:
	if ( lft.width + rgt.width ) > &columns
		let oversized
		\ = lft.width < lft.half ? [ [ rgt, &columns - lft.width ] ]
		\ : rgt.width < rgt.half ? [ [ lft, &columns - rgt.width ] ]
		\ :                        [ [ lft, lft.half ], [ rgt, rgt.half ] ]
		for [side, budget] in oversized
			let delta = side.width - budget
			" toss entire tabs to close the distance
			while delta >= tabs[side.lasttab].width
				let delta -= remove(tabs, side.lasttab).width
			endwhile
			" then snip at the last one to make it fit
			let endtab = tabs[side.lasttab]
			while delta > ( endtab.width - strwidth(strtrans(endtab.label)) )
				let endtab.label = substitute(endtab.label, side.cut, '', '')
			endwhile
			let endtab.label = substitute(endtab.label, side.cut, side.indicator, '')
		endfor
	endif

	if len(tabs) | let tabs[0].label = substitute(tabs[0].label, lpad, ' ', '') | endif

	let swallowclicks = '%'.(1 + tabpagenr('$')).'X'
	return s:tablineat
		\ ? join(map(tabs,'"%#BufTabLine".v:val.hilite."#" . "%".v:val.num."@'.s:sid.'switch_buffer@" . strtrans(v:val.label)'),'') . '%#BufTabLineFill#' . swallowclicks
		\ : swallowclicks . join(map(tabs,'"%#BufTabLine".v:val.hilite."#" . strtrans(v:val.label)'),'') . '%#BufTabLineFill#'
endfunction

function! s:buftabline_update(zombie)
	set tabline=
	if tabpagenr('$') > 1 | set guioptions+=e showtabline=2 | return | endif
	set guioptions-=e
	if 0 == g:buftabline_show
		set showtabline=1
		return
	elseif 1 == g:buftabline_show
		" account for BufDelete triggering before buffer is actually deleted
		let bufnums = filter(s:buftabline_user_buffers(), 'v:val != a:zombie')
		let &g:showtabline = 1 + ( len(bufnums) > 1 )
	elseif 2 == g:buftabline_show
		set showtabline=2
	endif
	set tabline=%!__buftabline_render()
endfunction

augroup BufTabLine
autocmd!
autocmd VimEnter  * call s:buftabline_update(0)
autocmd TabEnter  * call s:buftabline_update(0)
autocmd BufAdd    * call s:buftabline_update(0)
autocmd FileType qf call s:buftabline_update(0)
autocmd BufDelete * call s:buftabline_update(str2nr(expand('<abuf>')))
augroup END

for s:n in range(1, g:buftabline_plug_max) + ( g:buftabline_plug_max > 0 ? [-1] : [] )
	let s:b = s:n == -1 ? -1 : s:n - 1
    let s:n = s:n == -1 ? 20 : s:n
	execute printf("command! BufTabLineGo%d :exe 'b'.get(s:buftabline_user_buffers(),%d,'')", s:n, s:b)
endfor
unlet! s:n s:b

if v:version < 703
	function s:transpile()
		let [ savelist, &list ] = [ &list, 0 ]
		redir => src
			silent function __buftabline_render
		redir END
		let &list = savelist
		let src = substitute(src, '\n\zs[0-9 ]*', '', 'g')
		let src = substitute(src, 'strwidth(strtrans(\([^)]\+\)))', 'strlen(substitute(\1, ''\p\|\(.\)'', ''x\1'', ''g''))', 'g')
		return src
	endfunction
	exe "delfunction __buftabline_render\n" . s:transpile()
	delfunction s:transpile
endif
" }}}1



" repeat.vim - Let the repeat command repeat plugin maps {{{1
" Maintainer:   Tim Pope
" Version:      1.2
" GetLatestVimScripts: 2136 1 :AutoInstall: repeat.vim

" Installation:
" Place in either ~/.vim/plugin/repeat.vim (to load at start up) or
" ~/.vim/autoload/repeat.vim (to load automatically as needed).
"
" License:
" Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
" See :help license
"
" Developers:
" Basic usage is as follows:
"
"   silent! call __repeat_set("\<Plug>MappingToRepeatCommand",3)
"
" The first argument is the mapping that will be invoked when the |.| key is
" pressed.  Typically, it will be the same as the mapping the user invoked.
" This sequence will be stuffed into the input queue literally.  Thus you must
" encode special keys by prefixing them with a backslash inside double quotes.
"
" The second argument is the default count.  This is the number that will be
" prefixed to the mapping if no explicit numeric argument was given.  The
" value of the v:count variable is usually correct and it will be used if the
" second parameter is omitted.  If your mapping doesn't accept a numeric
" argument and you never want to receive one, pass a value of -1.
"
" Make sure to call the __repeat_set function _after_ making changes to the
" file.
"
" For mappings that use a register and want the same register used on
" repetition, use:
"
"   silent! call __repeat_setreg("\<Plug>MappingToRepeatCommand", v:register)
"
" This function can (and probably needs to be) called before making changes to
" the file (as those typically clear v:register).  Therefore, the call sequence
" in your mapping will look like this:
"
"   nnoremap <silent> <Plug>MyMap
"   \   :<C-U>execute 'silent! call __repeat_setreg("\<lt>Plug>MyMap", v:register)'<Bar>
"   \   call <SID>MyFunction(v:register, ...)<Bar>
"   \   silent! call __repeat_set("\<lt>Plug>MyMap")<CR>

if exists("g:loaded_repeat") || &cp || v:version < 700
    finish
endif
let g:loaded_repeat = 1

let g:repeat_tick = -1
let g:repeat_reg = ['', '']

" Special function to avoid spurious repeats in a related, naturally repeating
" mapping when your repeatable mapping doesn't increase b:changedtick.
function! __repeat_invalidate()
    autocmd! repeat_custom_motion
    let g:repeat_tick = -1
endfunction

function! __repeat_set(sequence,...)
    let g:repeat_sequence = a:sequence
    let g:repeat_count = a:0 ? a:1 : v:count
    let g:repeat_tick = b:changedtick
    augroup repeat_custom_motion
        autocmd!
        autocmd CursorMoved <buffer> let g:repeat_tick = b:changedtick | autocmd! repeat_custom_motion
    augroup END
endfunction

function! __repeat_setreg(sequence,register)
    let g:repeat_reg = [a:sequence, a:register]
endfunction


function! s:default_register()
    let values = split(&clipboard, ',')
    if index(values, 'unnamedplus') != -1
        return '+'
    elseif index(values, 'unnamed') != -1
        return '*'
    else
        return '"'
    endif
endfunction

function! __repeat_run(count)
    try
        if g:repeat_tick == b:changedtick
            let r = ''
            if g:repeat_reg[0] ==# g:repeat_sequence && !empty(g:repeat_reg[1])
                " Take the original register, unless another (non-default, we
                " unfortunately cannot detect no vs. a given default register)
                " register has been supplied to the repeat command (as an
                " explicit override).
                let regname = v:register ==# s:default_register() ? g:repeat_reg[1] : v:register
                if regname ==# '='
                    " This causes a re-evaluation of the expression on repeat, which
                    " is what we want.
                    let r = '"=' . getreg('=', 1) . "\<CR>"
                else
                    let r = '"' . regname
                endif
            endif

            let c = g:repeat_count
            let s = g:repeat_sequence
            let cnt = c == -1 ? "" : (a:count ? a:count : (c ? c : ''))
            if ((v:version == 703 && has('patch100')) || (v:version == 704 && !has('patch601')))
                exe 'norm ' . r . cnt . s
            elseif v:version <= 703
                call feedkeys(r . cnt, 'n')
                call feedkeys(s, '')
            else
                call feedkeys(s, 'i')
                call feedkeys(r . cnt, 'ni')
            endif
        else
            if ((v:version == 703 && has('patch100')) || (v:version == 704 && !has('patch601')))
                exe 'norm! '.(a:count ? a:count : '') . '.'
            else
                call feedkeys((a:count ? a:count : '') . '.', 'ni')
            endif
        endif
    catch /^Vim(normal):/
        return 'echoerr v:errmsg'
    endtry
    return ''
endfunction

function! __repeat_wrap(command,count)
    let preserve = (g:repeat_tick == b:changedtick)
    call feedkeys((a:count ? a:count : '').a:command, 'n')
    exe (&foldopen =~# 'undo\|all' ? 'norm! zv' : '')
    if preserve
        let g:repeat_tick = b:changedtick
    endif
endfunction

nnoremap <silent> <Plug>(RepeatDot)      :<C-U>exe __repeat_run(v:count)<CR>
nnoremap <silent> <Plug>(RepeatUndo)     :<C-U>call __repeat_wrap('u',v:count)<CR>
nnoremap <silent> <Plug>(RepeatUndoLine) :<C-U>call __repeat_wrap('U',v:count)<CR>
nnoremap <silent> <Plug>(RepeatRedo)     :<C-U>call __repeat_wrap("\<Lt>C-R>",v:count)<CR>

if !hasmapto('<Plug>(RepeatDot)', 'n')
    nmap . <Plug>(RepeatDot)
endif
if !hasmapto('<Plug>(RepeatUndo)', 'n')
    nmap u <Plug>(RepeatUndo)
endif
if maparg('U','n') ==# '' && !hasmapto('<Plug>(RepeatUndoLine)', 'n')
    nmap U <Plug>(RepeatUndoLine)
endif
if !hasmapto('<Plug>(RepeatRedo)', 'n')
    nmap <C-R> <Plug>(RepeatRedo)
endif

augroup repeatPlugin
    autocmd!
    autocmd BufLeave,BufWritePre,BufReadPre * let g:repeat_tick = (g:repeat_tick == b:changedtick || g:repeat_tick == 0) ? 0 : -1
    autocmd BufEnter,BufWritePost * if g:repeat_tick == 0|let g:repeat_tick = b:changedtick|endif
augroup END

" }}}1
