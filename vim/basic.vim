" Basic vimrc all in one file

" Quick install commmand for Unix:
" curl https://raw.githubusercontent.com/richyliu/config/master/vim/basic.vim -o myvimrc.vim
"
" Usage:
" vim -u myvimrc.vim [FILE]

set nocompatible

colorscheme default

" Settings {{{1
set autoindent
set background=light
set backspace=indent,eol,start
set breakindent
set breakindentopt=min:20,sbr
set clipboard=
set cursorline
set display+=truncate
set expandtab
set foldmethod=marker
set hidden
set history=10000
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set linebreak
set list
set listchars=tab:>\ ,trail:-,extends:>,precedes:<
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
set shortmess-=S
set showbreak=^
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
set viminfo=
set wildignore+=**/node_modules/**
set wildignorecase
set wildmenu
set wildmode=longest:full,full
set nowrapscan

filetype plugin on
syntax on

let g:buftabline_indicators=1
let g:buftabline_numbers=2

" delimitMate auto cloing options
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1

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
" close buffer
nnoremap <leader>d :<c-u>bdelete<cr>
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

" easy buffer switching
nnoremap <silent> <F8> :bn<CR>
nnoremap <silent> <F7> :bp<CR>

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

" Buffer switching with alt-num
nmap <M-1> :<c-u>BufTabLineGo1<cr>
nmap <M-2> :<c-u>BufTabLineGo2<cr>
nmap <M-3> :<c-u>BufTabLineGo3<cr>
nmap <M-4> :<c-u>BufTabLineGo4<cr>
nmap <M-5> :<c-u>BufTabLineGo5<cr>
nmap <M-6> :<c-u>BufTabLineGo6<cr>
nmap <M-7> :<c-u>BufTabLineGo7<cr>
nmap <M-8> :<c-u>BufTabLineGo8<cr>
nmap <M-9> :<c-u>BufTabLineGo20<cr>

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



" delimitMate - auto closing {{{1
" File:        autoload/delimitMate.vim
" Version:     2.7
" Modified:    2013-07-15
" Description: This plugin provides auto-completion for quotes, parens, etc.
" Maintainer:  Israel Chauca F. <israelchauca@gmail.com>
" Manual:      Read ":help delimitMate".
" ============================================================================

"let delimitMate_loaded = 1

if !exists('s:options')
  let s:options = {}
endif

function! s:set(name, value) "{{{
  let bufnr = bufnr('%')
  if !has_key(s:options, bufnr)
    let s:options[bufnr] = {}
  endif
  let s:options[bufnr][a:name] = a:value
endfunction "}}}

function! s:get(...) "{{{
  let options = deepcopy(eval('s:options.' . bufnr('%')))
  if a:0
    return options[a:1]
  endif
  return options
endfunction "}}}

function! s:exists(name, ...) "{{{
  let scope = a:0 ? a:1 : 's'
  if scope == 's'
    let bufnr = bufnr('%')
    let name = 'options.' . bufnr . '.' . a:name
  else
    let name = 'delimitMate_' . a:name
  endif
  return exists(scope . ':' . name)
endfunction "}}}

function! s:is_jump(...) "{{{
  " Returns 1 if the next character is a closing delimiter.
  let char = s:get_char(0)
  let list = s:get('right_delims') + s:get('quotes_list')

  " Closing delimiter on the right.
  if (!a:0 && index(list, char) > -1)
        \ || (a:0 && char == a:1)
    return 1
  endif

  " Closing delimiter with space expansion.
  let nchar = s:get_char(1)
  if !a:0 && s:get('expand_space') && char == " "
    if index(list, nchar) > -1
      return 2
    endif
  elseif a:0 && s:get('expand_space') && nchar == a:1 && char == ' '
    return 3
  endif

  if !s:get('jump_expansion')
    return 0
  endif

  " Closing delimiter with CR expansion.
  let uchar = matchstr(getline(line('.') + 1), '^\s*\zs\S')
  if !a:0 && s:get('expand_cr') && char == ""
    if index(list, uchar) > -1
      return 4
    endif
  elseif a:0 && s:get('expand_cr') && uchar == a:1
    return 5
  endif
  return 0
endfunction "}}}

function! s:rquote(char) "{{{
  let pos = matchstr(getline('.')[col('.') : ], escape(a:char, '[]*.^$\'), 1)
  let i = 0
  while s:get_char(i) ==# a:char
    let i += 1
  endwhile
  return i
endfunction "}}}

function! s:lquote(char) "{{{
  let i = 0
  while s:get_char(i - 1) ==# a:char
    let i -= 1
  endwhile
  return i * -1
endfunction "}}}

function! s:get_char(...) "{{{
  let idx = col('.') - 1
  if !a:0 || (a:0 && a:1 >= 0)
    " Get char from cursor.
    let line = getline('.')[idx :]
    let pos = a:0 ? a:1 : 0
    return matchstr(line, '^'.repeat('.', pos).'\zs.')
  endif
  " Get char behind cursor.
  let line = getline('.')[: idx - 1]
  let pos = 0 - (1 + a:1)
  return matchstr(line, '.\ze'.repeat('.', pos).'$')
endfunction "s:get_char }}}

function! s:is_cr_expansion(...) " {{{
  let nchar = getline(line('.')-1)[-1:]
  let schar = matchstr(getline(line('.')+1), '^\s*\zs\S')
  let isEmpty = a:0 ? getline('.') =~ '^\s*$' : empty(getline('.'))
  if index(s:get('left_delims'), nchar) > -1
        \ && index(s:get('left_delims'), nchar)
        \    == index(s:get('right_delims'), schar)
        \ && isEmpty
    return 1
  elseif index(s:get('quotes_list'), nchar) > -1
        \ && index(s:get('quotes_list'), nchar)
        \    == index(s:get('quotes_list'), schar)
        \ && isEmpty
    return 1
  else
    return 0
  endif
endfunction " }}} s:is_cr_expansion()

function! s:is_space_expansion() " {{{
  if col('.') > 2
    let pchar = s:get_char(-2)
    let nchar = s:get_char(1)
    let isSpaces =
          \ (s:get_char(-1)
          \   == s:get_char(0)
          \ && s:get_char(-1) == " ")

    if index(s:get('left_delims'), pchar) > -1 &&
        \ index(s:get('left_delims'), pchar)
        \   == index(s:get('right_delims'), nchar) &&
        \ isSpaces
      return 1
    elseif index(s:get('quotes_list'), pchar) > -1 &&
        \ index(s:get('quotes_list'), pchar)
        \   == index(s:get('quotes_list'), nchar) &&
        \ isSpaces
      return 1
    endif
  endif
  return 0
endfunction " }}} IsSpaceExpansion()

function! s:is_empty_matchpair() "{{{
  " get char before the cursor.
  let open = s:get_char(-1)
  let idx = index(s:get('left_delims'), open)
  if idx == -1
    return 0
  endif
  let close = get(s:get('right_delims'), idx, '')
  return close ==# s:get_char(0)
endfunction "}}}

function! s:is_empty_quotes() "{{{
  " get char before the cursor.
  let quote = s:get_char(-1)
  let idx = index(s:get('quotes_list'), quote)
  if idx == -1
    return 0
  endif
  return quote ==# s:get_char(0)
endfunction "}}}

function! s:cursor_idx() "{{{
  let idx = len(split(getline('.')[: col('.') - 1], '\zs')) - 1
  return idx
endfunction "DelimitMate__CursorCol }}}

function! s:get_syn_name() "{{{
  let col = col('.')
  if  col == col('$')
    let col = col - 1
  endif
  return synIDattr(synIDtrans(synID(line('.'), col, 1)), 'name')
endfunction " }}}

function! s:is_excluded_ft(ft) "{{{
  if !exists("g:delimitMate_excluded_ft")
    return 0
  endif
  return index(split(g:delimitMate_excluded_ft, ','), a:ft, 0, 1) >= 0
endfunction "}}}

function! s:is_forbidden(char) "{{{
  if s:is_excluded_ft(&filetype)
    return 1
  endif
  if !s:get('excluded_regions_enabled')
    return 0
  endif
  let region = s:get_syn_name()
  return index(s:get('excluded_regions_list'), region) >= 0
endfunction "}}}

function! s:balance_matchpairs(char) "{{{
  " Returns:
  " = 0 => Parens balanced.
  " > 0 => More opening parens.
  " < 0 => More closing parens.

  let line = getline('.')
  let col = s:cursor_idx() - 1
  let col = col >= 0 ? col : 0
  let list = split(line, '\zs')
  let left = s:get('left_delims')[index(s:get('right_delims'), a:char)]
  let right = a:char
  let opening = 0
  let closing = 0

  " If the cursor is not at the beginning, count what's behind it.
  if col > 0
      " Find the first opening paren:
      let start = index(list, left)
      " Must be before cursor:
      let start = start < col ? start : col - 1
      " Now count from the first opening until the cursor, this will prevent
      " extra closing parens from being counted.
      let opening = count(list[start : col - 1], left)
      let closing = count(list[start : col - 1], right)
      " I don't care if there are more closing parens than opening parens.
      let closing = closing > opening ? opening : closing
  endif

  " Evaluate parens from the cursor to the end:
  let opening += count(list[col :], left)
  let closing += count(list[col :], right)

  " Return the found balance:
  return opening - closing
endfunction "}}}

function! s:is_smart_quote(char) "{{{
  " TODO: Allow using a:char in the pattern.
  let tmp = s:get('smart_quotes')
  if empty(tmp)
    return 0
  endif
  let regex = matchstr(tmp, '^!\?\zs.*')
  " Flip matched value if regex starts with !
  let mod = tmp =~ '^!' ? [1, 0] : [0, 1]
  let matched = search(regex, 'ncb', line('.')) > 0
  let noescaped = substitute(getline('.'), '\\.', '', 'g')
  let odd =  (count(split(noescaped, '\zs'), a:char) % 2)
  let result = mod[matched] || odd
  return result
endfunction "DelimitMate__SmartQuote }}}

function! g:DelimitMate__Set(...) "{{{
  return call('s:set', a:000)
endfunction "}}}

function! g:DelimitMate__Get(...) "{{{
  return call('s:get', a:000)
endfunction "}}}

function! g:DelimitMate__ShouldJump(...) "{{{
  return call('s:is_jump', a:000)
endfunction "}}}

function! g:DelimitMate__IsEmptyPair(str) "{{{
  if strlen(substitute(a:str, ".", "x", "g")) != 2
    return 0
  endif
  let idx = index(s:get('left_delims'), matchstr(a:str, '^.'))
  if idx > -1 &&
        \ s:get('right_delims')[idx] == matchstr(a:str, '.$')
    return 1
  endif
  let idx = index(s:get('quotes_list'), matchstr(a:str, '^.'))
  if idx > -1 &&
        \ s:get('quotes_list')[idx] == matchstr(a:str, '.$')
    return 1
  endif
  return 0
endfunction "}}}

function! g:DelimitMate__WithinEmptyPair() "{{{
  " if cursor is at column 1 return 0
  if col('.') == 1
    return 0
  endif
  " get char before the cursor.
  let char1 = s:get_char(-1)
  " get char under the cursor.
  let char2 = s:get_char(0)
  return DelimitMate__IsEmptyPair( char1.char2 )
endfunction "}}}

function! g:DelimitMate__SkipDelim(char) "{{{
  if s:is_forbidden(a:char)
    return a:char
  endif
  let col = col('.') - 1
  let line = getline('.')
  if col > 0
    let cur = s:get_char(0)
    let pre = s:get_char(-1)
  else
    let cur = s:get_char(0)
    let pre = ""
  endif
  if pre == "\\"
    " Escaped character
    return a:char
  elseif cur == a:char
    " Exit pair
    return a:char . "\<Del>"
  elseif DelimitMate__IsEmptyPair( pre . a:char )
    " Add closing delimiter and jump back to the middle.
    return a:char . s:joinUndo() . "\<Left>"
  else
    " Nothing special here, return the same character.
    return a:char
  endif
endfunction "}}}

function! g:DelimitMate__ParenDelim(right) " {{{
  let left = s:get('left_delims')[index(s:get('right_delims'),a:right)]
  if s:is_forbidden(a:right)
    return left
  endif
  " Try to balance matchpairs
  if s:get('balance_matchpairs') &&
        \ s:balance_matchpairs(a:right) < 0
    return left
  endif
  let line = getline('.')
  let col = col('.')-2
  if s:get('smart_matchpairs') != ''
    let smart_matchpairs = substitute(s:get('smart_matchpairs'), '\\!', left, 'g')
    let smart_matchpairs = substitute(smart_matchpairs, '\\#', a:right, 'g')
    if line[col+1:] =~ smart_matchpairs
      return left
    endif
  endif
  if len(line) == (col + 1) && s:get('insert_eol_marker') == 1
    let tail = s:get('eol_marker')
  else
    let tail = ''
  endif
  return left . a:right . tail . repeat(s:joinUndo() . "\<Left>", len(split(tail, '\zs')) + 1)
endfunction " }}}

function! g:DelimitMate__QuoteDelim(char) "{{{
  if s:is_forbidden(a:char)
    return a:char
  endif
  let char_at = s:get_char(0)
  let char_before = s:get_char(-1)
  let nesting_on = index(s:get('nesting_quotes'), a:char) > -1
  let left_q = nesting_on ? s:lquote(a:char) : 0
  if nesting_on && left_q > 1
    " Nesting quotes.
    let right_q =  s:rquote(a:char)
    let quotes = right_q > left_q + 1 ? 0 : left_q - right_q + 2
    let lefts = quotes - 1
    return repeat(a:char, quotes) . repeat(s:joinUndo() . "\<Left>", lefts)
  elseif char_at == a:char
    " Inside an empty pair, jump out
    return a:char . "\<Del>"
  elseif a:char == '"' && index(split(&ft, '\.'), "vim") != -1 && getline('.') =~ '^\s*$'
    " If we are in a vim file and it looks like we're starting a comment, do
    " not add a closing char.
    return a:char
  elseif s:is_smart_quote(a:char)
    " Seems like a smart quote, insert a single char.
    return a:char
  elseif (char_before == a:char && char_at != a:char)
        \ && !empty(s:get('smart_quotes'))
    " Seems like we have an unbalanced quote, insert one quotation
    " mark and jump to the middle.
    return a:char . s:joinUndo() . "\<Left>"
  else
    " Insert a pair and jump to the middle.
    let sufix = ''
    if !empty(s:get('eol_marker')) && col('.') - 1 == len(getline('.'))
      let idx = len(s:get('eol_marker')) * -1
      let marker = getline('.')[idx : ]
      let has_marker = marker == s:get('eol_marker')
      let sufix = !has_marker ? s:get('eol_marker') : ''
    endif
    return a:char . a:char . s:joinUndo() . "\<Left>"
  endif
endfunction "}}}

function! g:DelimitMate__JumpOut(char) "{{{
  if s:is_forbidden(a:char)
    return a:char
  endif
  let jump = s:is_jump(a:char)
  if jump == 1
    " HACK: Instead of <Right>, we remove the char to be jumped over and
    " insert it again. This will trigger re-indenting via 'indentkeys'.
    " Ref: https://github.com/Raimondi/delimitMate/issues/168
    return "\<Del>".a:char
  elseif jump == 3
    return s:joinUndo() . "\<Right>" . s:joinUndo() . "\<Right>"
  elseif jump == 5
    return "\<Down>\<C-O>I" . s:joinUndo() . "\<Right>"
  else
    return a:char
  endif
endfunction " }}}

function! g:DelimitMate__JumpAny(...) " {{{
  if s:is_forbidden('')
    return ''
  endif
  if !s:is_jump()
    return ''
  endif
  " Let's get the character on the right.
  let char = s:get_char(0)
  if char == " "
    " Space expansion.
    return s:joinUndo() . "\<Right>" . s:joinUndo() . "\<Right>"
  elseif char == ""
    " CR expansion.
    return "\<CR>" . getline(line('.') + 1)[0] . "\<Del>\<Del>"
  else
    return s:joinUndo() . "\<Right>"
  endif
endfunction " DelimitMate__JumpAny() }}}

function! g:DelimitMate__JumpMany() " {{{
  let line = split(getline('.')[col('.') - 1 : ], '\zs')
  let rights = ""
  let found = 0
  for char in line
    if index(s:get('quotes_list'), char) >= 0 ||
          \ index(s:get('right_delims'), char) >= 0
      let rights .= s:joinUndo() . "\<Right>"
      let found = 1
    elseif found == 0
      let rights .= s:joinUndo() . "\<Right>"
    else
      break
    endif
  endfor
  if found == 1
    return rights
  else
    return ''
  endif
endfunction " DelimitMate__JumpMany() }}}

function! g:DelimitMate__ExpandReturn() "{{{
  if s:is_forbidden("")
    return "\<CR>"
  endif
  let escaped = s:cursor_idx() >= 2
        \ && s:get_char(-2) == '\'
  let expand_right_matchpair = s:get('expand_cr') == 2
        \     && index(s:get('right_delims'), s:get_char(0)) > -1
  let expand_inside_quotes = s:get('expand_inside_quotes')
          \     && s:is_empty_quotes()
          \     && !escaped
  let is_empty_matchpair = s:is_empty_matchpair()
  if !pumvisible(  )
        \ && (   is_empty_matchpair
        \     || expand_right_matchpair
        \     || expand_inside_quotes)
    let val = "\<Esc>a"
    if is_empty_matchpair && s:get('insert_eol_marker') == 2
          \ && !search(escape(s:get('eol_marker'), '[]\.*^$').'$', 'cnW', '.')
      let tail = getline('.')[col('.') - 1 : ]
      let times = len(split(tail, '\zs'))
      let val .= repeat(s:joinUndo() . "\<Right>", times) . s:get('eol_marker') . repeat(s:joinUndo() . "\<Left>", times + 1)
    endif
    let val .= "\<CR>"
    if &smartindent && !&cindent && !&indentexpr
          \ && s:get_char(0) == '}'
      " indentation is controlled by 'smartindent', and the first character on
      " the new line is '}'. If this were typed manually it would reindent to
      " match the current line. Let's reproduce that behavior.
      let shifts = indent('.') / &sw
      let spaces = indent('.') - (shifts * &sw)
      let val .= "^\<C-D>".repeat("\<C-T>", shifts).repeat(' ', spaces)
    endif
    " Expand:
    " XXX zv prevents breaking expansion with syntax folding enabled by
    " InsertLeave.
    let val .= "\<Esc>zvO"
    return val
  else
    return "\<CR>"
  endif
endfunction "}}}

function! g:DelimitMate__ExpandSpace() "{{{
  if s:is_forbidden("\<Space>")
    return "\<Space>"
  endif
  let escaped = s:cursor_idx() >= 2
        \ && s:get_char(-2) == '\'
  let expand_inside_quotes = s:get('expand_inside_quotes')
          \     && s:is_empty_quotes()
          \     && !escaped
  if s:is_empty_matchpair() || expand_inside_quotes
    " Expand:
    return "\<Space>\<Space>" . s:joinUndo() . "\<Left>"
  else
    return "\<Space>"
  endif
endfunction "}}}

function! g:DelimitMate__BS() " {{{
  if s:is_forbidden("")
    let extra = ''
  elseif &bs !~ 'start\|2'
    let extra = ''
  elseif DelimitMate__WithinEmptyPair()
    let extra = "\<Del>"
  elseif s:is_space_expansion()
    let extra = "\<Del>"
  elseif s:is_cr_expansion()
    let extra = repeat("\<Del>",
          \ len(matchstr(getline(line('.') + 1), '^\s*\S')))
  else
    let extra = ''
  endif
  return "\<BS>" . extra
endfunction " }}} DelimitMate__BS()

function! g:DelimitMate__Test() "{{{
  %d _
  " Check for script options:
  let result = [
        \ 'delimitMate Report',
        \ '==================',
        \ '',
        \ '* Options: ( ) default, (g) global, (b) buffer',
        \ '']
  for option in sort(keys(s:options[bufnr('%')]))
    if s:exists(option, 'b')
      let scope = '(b)'
    elseif s:exists(option, 'g')
      let scope = '(g)'
    else
      let scope = '( )'
    endif
    call add(result,
          \ scope . ' delimitMate_' . option
          \ . ' = '
          \ . string(s:get(option)))
  endfor
  call add(result, '')

  let option = 'delimitMate_excluded_ft'
  call add(result,
        \(exists('g:'.option) ? '(g) ' : '( ) g:') . option . ' = '
        \. string(get(g:, option, '')))

  call add(result, '--------------------')
  call add(result, '')

  " Check if mappings were set.
  let left_delims = s:get('autoclose') ? s:get('left_delims') : []
  let special_keys = ['<BS>', '<S-BS>', '<S-Tab>', '<C-G>g']
  if s:get('expand_cr')
    call add(special_keys, '<CR>')
  endif
  if s:get('expand_space')
    call add(special_keys, '<Space>')
  endif
  let maps =
        \ s:get('right_delims')
        \ + left_delims
        \ + s:get('quotes_list')
        \ + s:get('apostrophes_list')
        \ + special_keys

  call add(result, '* Mappings:')
  call add(result, '')
  for map in maps
    let output = ''
    if map == '|'
      let map = '<Bar>'
    endif
    redir => output | execute "verbose imap ".map | redir END
    call extend(result, split(output, '\n'))
  endfor

  call add(result, '--------------------')
  call add(result, '')
  call add(result, '* Showcase:')
  call add(result, '')
  call setline(1, result)
  call s:test_mappings(s:get('left_delims'), 1)
  call s:test_mappings(s:get('quotes_list'), 0)

  let result = []
  redir => setoptions
  echo " * Vim configuration:\<NL>"
  filetype
  echo ""
  set
  version
  redir END
  call extend(result, split(setoptions,"\n"))
  call add(result, '--------------------')
  setlocal nowrap
  call append('$', result)
  call feedkeys("\<Esc>\<Esc>", 'n')
endfunction "}}}

function! s:test_mappings(list, is_matchpair) "{{{
  let prefix = "normal Go0\<C-D>"
  let last = "|"
  let open = s:get('autoclose') ? 'Open: ' : 'Open & close: '
  for s in a:list
    if a:is_matchpair
      let pair = s:get('right_delims')[index(s:get('left_delims'), s)]
    else
      let pair = s
    endif
    if !s:get('autoclose')
      let s .= pair
    endif
    exec prefix . open . s . last
    exec prefix . "Delete: " . s . "\<BS>" . last
    exec prefix . "Exit: " . s . pair . last
    if s:get('expand_space')
          \ && (a:is_matchpair || s:get('expand_inside_quotes'))
      exec prefix . "Space: " . s . " " . last
      exec prefix . "Delete space: " . s . " \<BS>" . last
    endif
    if s:get('expand_cr')
          \ && (a:is_matchpair || s:get('expand_inside_quotes'))
      exec prefix . "Car return: " . s . "\<CR>" . last
      exec prefix . "Delete car return: " . s . "\<CR>0\<C-D>\<BS>" . last
    endif
    call append('$', '')
  endfor
endfunction "}}}

function! s:joinUndo() "{{{
  if v:version < 704
        \ || ( v:version == 704 && !has('patch849') )
    return ''
  endif
  return "\<C-G>U"
endfunction "}}}

" vim:foldmethod=marker:foldcolumn=4:ts=2:sw=2

" File:        plugin/delimitMate.vim
" Version:     2.7
" Modified:    2013-07-15
" Description: This plugin provides auto-completion for quotes, parens, etc.
" Maintainer:  Israel Chauca F. <israelchauca@gmail.com>
" Manual:      Read ":help delimitMate".
" ============================================================================

" Initialization: {{{

if exists("g:loaded_delimitMate") || &cp
  " User doesn't want this plugin or compatible is set, let's get out!
  finish
endif
let g:loaded_delimitMate = 1
let save_cpo = &cpo
set cpo&vim

if v:version < 700
  echoerr "delimitMate: this plugin requires vim >= 7!"
  finish
endif

let s:loaded_delimitMate = 1
let delimitMate_version = "2.8"

"}}}

" Functions: {{{

function! s:option_init(name, default) "{{{
  let opt_name = "delimitMate_" . a:name
  " Find value to use.
  if !has_key(b:, opt_name) && !has_key(g:, opt_name)
    let value = a:default
  elseif has_key(b:, opt_name)
    let value = b:[opt_name]
  else
    let value = g:[opt_name]
  endif
  call s:set(a:name, value)
endfunction "}}}

function! s:init() "{{{
" Initialize variables:
  " autoclose
  call s:option_init("autoclose", 1)
  " matchpairs
  call s:option_init("matchpairs", string(&matchpairs)[1:-2])
  call s:option_init("matchpairs_list", map(split(s:get('matchpairs', ''), '.:.\zs,\ze.:.'), 'split(v:val, ''^.\zs:\ze.$'')'))
  let pairs = s:get('matchpairs_list', [])
  if len(filter(pairs, 'v:val[0] ==# v:val[1]'))
    echohl ErrorMsg
    echom 'delimitMate: each member of a pair in delimitMate_matchpairs must be different from each other.'
    echom 'delimitMate: invalid pairs: ' . join(map(pairs, 'join(v:val, ":")'), ', ')
    echohl Normal
    return 0
  endif
  call s:option_init("left_delims", map(copy(s:get('matchpairs_list', [])), 'v:val[0]'))
  call s:option_init("right_delims", map(copy(s:get('matchpairs_list', [])), 'v:val[1]'))
  " quotes
  call s:option_init("quotes", "\" ' `")
  call s:option_init("quotes_list",split(s:get('quotes', ''), '\s\+'))
  " nesting_quotes
  call s:option_init("nesting_quotes", [])
  " excluded_regions
  call s:option_init("excluded_regions", "Comment")
  call s:option_init("excluded_regions_list", split(s:get('excluded_regions', ''), ',\s*'))
  let enabled = len(s:get('excluded_regions_list', [])) > 0
  call s:option_init("excluded_regions_enabled", enabled)
  " expand_space
  if exists("b:delimitMate_expand_space") && type(b:delimitMate_expand_space) == type("")
    echom "b:delimitMate_expand_space is '".b:delimitMate_expand_space."' but it must be either 1 or 0!"
    echom "Read :help 'delimitMate_expand_space' for more details."
    unlet b:delimitMate_expand_space
    let b:delimitMate_expand_space = 1
  endif
  if exists("g:delimitMate_expand_space") && type(g:delimitMate_expand_space) == type("")
    echom "delimitMate_expand_space is '".g:delimitMate_expand_space."' but it must be either 1 or 0!"
    echom "Read :help 'delimitMate_expand_space' for more details."
    unlet g:delimitMate_expand_space
    let g:delimitMate_expand_space = 1
  endif
  call s:option_init("expand_space", 0)
  " expand_cr
  if exists("b:delimitMate_expand_cr") && type(b:delimitMate_expand_cr) == type("")
    echom "b:delimitMate_expand_cr is '".b:delimitMate_expand_cr."' but it must be either 1 or 0!"
    echom "Read :help 'delimitMate_expand_cr' for more details."
    unlet b:delimitMate_expand_cr
    let b:delimitMate_expand_cr = 1
  endif
  if exists("g:delimitMate_expand_cr") && type(g:delimitMate_expand_cr) == type("")
    echom "delimitMate_expand_cr is '".g:delimitMate_expand_cr."' but it must be either 1 or 0!"
    echom "Read :help 'delimitMate_expand_cr' for more details."
    unlet g:delimitMate_expand_cr
    let g:delimitMate_expand_cr = 1
  endif
  if ((&backspace !~ 'eol' || &backspace !~ 'start') && &backspace != 2) &&
        \ ((exists('b:delimitMate_expand_cr') && b:delimitMate_expand_cr == 1) ||
        \ (exists('g:delimitMate_expand_cr') && g:delimitMate_expand_cr == 1))
    echom "delimitMate: There seems to be some incompatibility with your settings that may interfer with the expansion of <CR>. See :help 'delimitMate_expand_cr' for details."
  endif
  call s:option_init("expand_cr", 0)
  " expand_in_quotes
  call s:option_init('expand_inside_quotes', 0)
  " jump_expansion
  call s:option_init("jump_expansion", 0)
  " smart_matchpairs
  call s:option_init("smart_matchpairs", '^\%(\w\|\!\|[£$]\|[^[:punct:][:space:]]\)')
  " smart_quotes
  " XXX: backward compatibility. Ugly, should go the way of the dodo soon.
  let quotes = escape(join(s:get('quotes_list', []), ''), '\-^[]')
  let default_smart_quotes = '\%(\w\|[^[:punct:][:space:]' . quotes . ']\|\%(\\\\\)*\\\)\%#\|\%#\%(\w\|[^[:space:][:punct:]' . quotes . ']\)'
  if exists('g:delimitMate_smart_quotes') && type(g:delimitMate_smart_quotes) == type(0)
    if g:delimitMate_smart_quotes
      unlet g:delimitMate_smart_quotes
    else
      unlet g:delimitMate_smart_quotes
      let g:delimitMate_smart_quotes = ''
    endif
  endif
  if exists('b:delimitMate_smart_quotes') && type(b:delimitMate_smart_quotes) == type(0)
    if b:delimitMate_smart_quotes
      unlet b:delimitMate_smart_quotes
      if exists('g:delimitMate_smart_quotes') && type(g:delimitMate_smart_quotes) && g:delimitMate_smart_quotes
        let b:delimitMate_smart_quotes = default_smart_quotes
      endif
    else
      unlet b:delimitMate_smart_quotes
      let b:delimitMate_smart_quotes = ''
    endif
  endif
  call s:option_init("smart_quotes", default_smart_quotes)
  " apostrophes
  call s:option_init("apostrophes", "")
  call s:option_init("apostrophes_list", split(s:get('apostrophes', ''), ":\s*"))
  " tab2exit
  call s:option_init("tab2exit", 1)
  " balance_matchpairs
  call s:option_init("balance_matchpairs", 0)
  " eol marker
  call s:option_init("insert_eol_marker", 1)
  call s:option_init("eol_marker", "")
  " Everything is fine.
  return 1
endfunction "}}} Init()

function! s:Map() "{{{
  " Set mappings:
  try
    let save_keymap = &keymap
    let save_iminsert = &iminsert
    let save_imsearch = &imsearch
    let save_cpo = &cpo
    set keymap=
    set cpo&vim
    silent! doautocmd <nomodeline> User delimitMate_map
    if s:get('autoclose', 1)
      call s:AutoClose()
    else
      call s:NoAutoClose()
    endif
    call s:ExtraMappings()
  finally
    let &cpo = save_cpo
    let &keymap = save_keymap
    let &iminsert = save_iminsert
    let &imsearch = save_imsearch
  endtry

  let b:delimitMate_enabled = 1
endfunction "}}} Map()

function! s:Unmap() " {{{
  let imaps =
        \ s:get('right_delims', []) +
        \ s:get('left_delims', []) +
        \ s:get('quotes_list', []) +
        \ s:get('apostrophes_list', []) +
        \ ['<BS>', '<C-h>', '<S-BS>', '<Del>', '<CR>', '<Space>', '<S-Tab>', '<Esc>'] +
        \ ['<Up>', '<Down>', '<Left>', '<Right>', '<LeftMouse>', '<RightMouse>'] +
        \ ['<C-Left>', '<C-Right>'] +
        \ ['<Home>', '<End>', '<PageUp>', '<PageDown>', '<S-Down>', '<S-Up>', '<C-G>g']

  for map in imaps
    if maparg(map, "i") =~# '^<Plug>delimitMate'
      if map == '|'
        let map = '<Bar>'
      endif
      exec 'silent! iunmap <buffer> ' . map
    endif
  endfor
  silent! doautocmd <nomodeline> User delimitMate_unmap
  let b:delimitMate_enabled = 0
endfunction " }}} s:Unmap()

function! s:test() "{{{
  if &modified
    let confirm = input("Modified buffer, type \"yes\" to write and proceed "
          \ . "with test: ") ==? 'yes'
    if !confirm
      return
    endif
  endif
  call DelimitMate__Test()
  g/\%^$/d
  0
endfunction "}}}

function! s:setup(...) "{{{
  let swap = a:0 && a:1 == 2
  let enable = a:0 && a:1
  let disable = a:0 && !a:1
  " First, remove all magic, if needed:
  if get(b:, 'delimitMate_enabled', 0)
    call s:Unmap()
    " Switch
    if swap
      echo "delimitMate is disabled."
      return
    endif
  endif
  if disable
    " Just disable the mappings.
    return
  endif
  if !a:0
    " Check if this file type is excluded:
    if exists("g:delimitMate_excluded_ft") &&
          \ index(split(g:delimitMate_excluded_ft, ','), &filetype, 0, 1) >= 0
      " Finish here:
      return 1
    endif
    " Check if user tried to disable using b:loaded_delimitMate
    if exists("b:loaded_delimitMate")
      return 1
    endif
  endif
  " Initialize settings:
  if ! s:init()
    " Something went wrong.
    return
  endif
  if enable || swap || !get(g:, 'delimitMate_offByDefault', 0)
    " Now, add magic:
    call s:Map()
    if a:0
      echo "delimitMate is enabled."
    endif
  endif
endfunction "}}}

function! s:TriggerAbb() "{{{
  if v:version < 703
        \ || ( v:version == 703 && !has('patch489') )
        \ || pumvisible()
    return ''
  endif
  return "\<C-]>"
endfunction "}}}

function! s:NoAutoClose() "{{{
  " inoremap <buffer> ) <C-R>=DelimitMate__SkipDelim('\)')<CR>
  for delim in s:get('right_delims', []) + s:get('quotes_list', [])
    if delim == '|'
      let delim = '<Bar>'
    endif
    exec 'inoremap <silent> <Plug>delimitMate' . delim . ' <C-R>=<SID>TriggerAbb().DelimitMate__SkipDelim("' . escape(delim,'"') . '")<CR>'
    exec 'silent! imap <unique> <buffer> '.delim.' <Plug>delimitMate'.delim
  endfor
endfunction "}}}

function! s:AutoClose() "{{{
  " Add matching pair and jump to the midle:
  " inoremap <silent> <buffer> ( ()<Left>
  let i = 0
  while i < len(s:get('matchpairs_list', []))
    let ld = s:get('left_delims', [])[i] == '|' ? '<bar>' : s:get('left_delims', [])[i]
    let rd = s:get('right_delims', [])[i] == '|' ? '<bar>' : s:get('right_delims', [])[i]
    exec 'inoremap <expr><silent> <Plug>delimitMate' . ld
                \. ' <SID>TriggerAbb().DelimitMate__ParenDelim("' . escape(rd, '|') . '")'
    exec 'silent! imap <unique> <buffer> '.ld
                \.' <Plug>delimitMate'.ld
    let i += 1
  endwhile

  " Exit from inside the matching pair:
  for delim in s:get('right_delims', [])
    let delim = delim == '|' ? '<bar>' : delim
    exec 'inoremap <expr><silent> <Plug>delimitMate' . delim
                \. ' <SID>TriggerAbb().DelimitMate__JumpOut("\' . delim . '")'
    exec 'silent! imap <unique> <buffer> ' . delim
                \. ' <Plug>delimitMate'. delim
  endfor

  " Add matching quote and jump to the midle, or exit if inside a pair of matching quotes:
  " inoremap <silent> <buffer> " <C-R>=DelimitMate__QuoteDelim("\"")<CR>
  for delim in s:get('quotes_list', [])
    if delim == '|'
      let delim = '<Bar>'
    endif
    exec 'inoremap <expr><silent> <Plug>delimitMate' . delim
                \. ' <SID>TriggerAbb()."<C-R>=DelimitMate__QuoteDelim(\"\\\' . delim . '\")<CR>"'
    exec 'silent! imap <unique> <buffer> ' . delim
                \. ' <Plug>delimitMate' . delim
  endfor

  " Try to fix the use of apostrophes (kept for backward compatibility):
  " inoremap <silent> <buffer> n't n't
  for map in s:get('apostrophes_list', [])
    exec "inoremap <silent> " . map . " " . map
    exec 'silent! imap <unique> <buffer> ' . map . ' <Plug>delimitMate' . map
  endfor
endfunction "}}}

function! s:ExtraMappings() "{{{
  " If pair is empty, delete both delimiters:
  inoremap <silent> <Plug>delimitMateBS <C-R>=DelimitMate__BS()<CR>
  if !hasmapto('<Plug>delimitMateBS','i')
    if empty(maparg('<BS>', 'i'))
      silent! imap <unique> <buffer> <BS> <Plug>delimitMateBS
    endif
    if empty(maparg('<C-H>', 'i'))
      silent! imap <unique> <buffer> <C-h> <Plug>delimitMateBS
    endif
  endif
  " If pair is empty, delete closing delimiter:
  inoremap <silent> <expr> <Plug>delimitMateS-BS DelimitMate__WithinEmptyPair() ? "\<Del>" : "\<S-BS>"
  if !hasmapto('<Plug>delimitMateS-BS','i') && maparg('<S-BS>', 'i') == ''
    silent! imap <unique> <buffer> <S-BS> <Plug>delimitMateS-BS
  endif
  " Expand return if inside an empty pair:
  inoremap <expr><silent> <Plug>delimitMateCR <SID>TriggerAbb()."\<C-R>=DelimitMate__ExpandReturn()\<CR>"
  if s:get('expand_cr', 0) && !hasmapto('<Plug>delimitMateCR', 'i') && maparg('<CR>', 'i') == ''
    silent! imap <unique> <buffer> <CR> <Plug>delimitMateCR
  endif
  " Expand space if inside an empty pair:
  inoremap <expr><silent> <Plug>delimitMateSpace <SID>TriggerAbb()."\<C-R>=DelimitMate__ExpandSpace()\<CR>"
  if s:get('expand_space', 0) && !hasmapto('<Plug>delimitMateSpace', 'i') && maparg('<Space>', 'i') == ''
    silent! imap <unique> <buffer> <Space> <Plug>delimitMateSpace
  endif
  " Jump over any delimiter:
  inoremap <expr><silent> <Plug>delimitMateS-Tab <SID>TriggerAbb()."\<C-R>=DelimitMate__JumpAny()\<CR>"
  if s:get('tab2exit', 0) && !hasmapto('<Plug>delimitMateS-Tab', 'i') && maparg('<S-Tab>', 'i') == ''
    silent! imap <unique> <buffer> <S-Tab> <Plug>delimitMateS-Tab
  endif
  " Jump over next delimiters
  inoremap <expr><buffer> <Plug>delimitMateJumpMany <SID>TriggerAbb()."\<C-R>=DelimitMate__JumpMany()\<CR>"
  if !hasmapto('<Plug>delimitMateJumpMany', 'i') && maparg("<C-G>g", 'i') == ''
    imap <silent> <buffer> <C-G>g <Plug>delimitMateJumpMany
  endif
endfunction "}}}

"}}}

" Commands: {{{

" Let me refresh without re-loading the buffer:
command! -bar DelimitMateReload call s:setup(1)
" Quick test:
command! -bar DelimitMateTest call s:test()
" Switch On/Off:
command! -bar DelimitMateSwitch call s:setup(2)
" Enable mappings:
command! -bar DelimitMateOn call s:setup(1)
" Disable mappings:
command! -bar DelimitMateOff call s:setup(0)

"}}}

" Autocommands: {{{

augroup delimitMate
  au!
  " Run on file type change.
  au FileType * call <SID>setup()
  au FileType python let b:delimitMate_nesting_quotes = ['"', "'"]

  " Run on new buffers.
  au BufNewFile,BufRead,BufEnter,CmdwinEnter *
        \ if !exists('b:delimitMate_was_here') |
        \   call <SID>setup() |
        \   let b:delimitMate_was_here = 1 |
        \ endif
augroup END

"}}}

" This is for the default buffer when it does not have a filetype.
call s:setup()

let &cpo = save_cpo
" GetLatestVimScripts: 2754 1 :AutoInstall: delimitMate.vim
" vim:foldmethod=marker:foldcolumn=4:ts=2:sw=2
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
