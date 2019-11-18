set nocompatible

set autoindent
set background=dark
set backspace=indent,eol,start
set clipboard=unnamed
set cursorline
set expandtab
set hidden
set hlsearch
set ignorecase
set incsearch
set nomodeline
set mouse=a
set nrformats-=octal
set number
set path=.,,**
set relativenumber
set ruler
set scrolloff=5
set shiftwidth=2
set showcmd
set smartcase
set softtabstop=2
set splitright
set nostartofline
set tabstop=4
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=10
set wildignore+=**/node_modules/**
set wildignorecase
set wildmenu
set wildmode=longest:full,full

filetype off
syntax on
filetype plugin on

let mapleader = " "
let maplocalleader = "L"
" Save all file
noremap <leader>s :w<cr>
" Source current file
noremap <leader>o :source %<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open file
nnoremap <leader>f :find 
" Save and close file
nnoremap <leader>z :call CloseFile()<cr>
nnoremap <leader>za :xa<cr>
" Split windows
nnoremap <leader>ws :split<cr>
nnoremap <leader>wv :vsplit<cr>
" Stop highlighting search
nnoremap <leader>n :nohlsearch<cr>
" Toggle between dark and light background
nnoremap <leader>b :let &background = ( &background == "dark" ? "light" : "dark" )<CR>
" cd to current file directory
nnoremap <leader>c :cd %:p:h<cr>
" yank current line to system clipboard
nnoremap <leader>y "+yy
" paste current line from system clipboard
nnoremap <leader>a "+pj

" No need for shift to type commands
nnoremap ; :
vnoremap ; :
" Move line forward or backward
nnoremap _ ddkP
nnoremap + ddp
" Cycle through buffers
nnoremap <tab> :bnext<cr>
nnoremap <s-tab> :bprevious<cr>
" Yank until end of line
nnoremap Y y$

" Delete with Ctrl-L (backspace w/ Ctrl-H)
inoremap <c-l> <del>
" Delete line in insert mode
inoremap <c-d> <c-o>dd
" Un-indent with Ctrl-F (b/c Ctrl-D doesn't work)
inoremap <c-f> <c-d>

" Go to previous command with Ctrl-K
cnoremap <c-k> <up>
nnoremap <c-k> :<up>
" Or next command with Ctrl-J
cnoremap <c-j> <down>
" Delete with alt-h
cnoremap ˙ <del>
" Go to beginning of line with ctrl-a
cnoremap <c-a> <c-b>

" change comma to semicolon (repeat previous t or f)
nnoremap , ;
vnoremap , ;

" Close and save: buffer, if >1 buffer, or file
function CloseFile()
  update
  if len(getbufinfo({'buflisted':1})) - 1
    bdelete
  else
    execute "normal! ZZ"
  endif
endfunction

" source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>



" Operator pending mappings
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap an( :<c-u>normal! f(va(<cr>
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap an{ :<c-u>normal! f{va{<cr>
onoremap in' :<c-u>normal! f'vi'<cr>
onoremap an' :<c-u>normal! f'va'<cr>
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap an" :<c-u>normal! f"va"<cr>
