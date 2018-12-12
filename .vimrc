set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'kshenoy/vim-signature'
Plugin 'ervandew/supertab'
Plugin 'prettier/vim-prettier'
Plugin 'jiangmiao/auto-pairs'
Plugin 'airblade/vim-gitgutter'

Plugin 'posva/vim-vue'
Plugin 'mxw/vim-jsx'
Plugin 'plasticboy/vim-markdown'

Plugin 'quramy/tsuquyomi'
Plugin 'leafgarland/typescript-vim'
Plugin 'vim-syntastic/syntastic'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" Plugin options {{{
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

let g:prettier#autoformat = 0
let g:prettier#config#parser = 'typescript'
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

let g:syntastic_typescript_checkers = ['tslint']

let g:tsuquyomi_completion_detail = 1
" }}}


" Settings {{{
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set backspace=indent,eol,start
set relativenumber
set title

set ruler
syntax on
set showcmd
set number
set hlsearch

set timeoutlen=500
set ttimeoutlen=0
set scrolloff=5
set smartcase
set wildmenu
set hidden

colorscheme monokai
" }}}


" Keymaps {{{
let mapleader = ","
let maplocalleader = "L"
" Save and prettify file
noremap <leader>s :write<cr>
" Source current file
noremap <leader>sv :source $MYVIMRC<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open vimrc file on a adjacent window
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
" Save and close file
noremap <leader>z :call CloseFile()<cr>

" No need for shift to type commands
nnoremap ; :
" Move line forward or backward
nnoremap _ ddkP
nnoremap + ddp
" Cycle through buffers
nnoremap <tab> :bnext<cr>
nnoremap <s-tab> :bprevious<cr>
" Go down a page
noremap f <c-d>

" Delete with Ctrl-L (backspace w/ Ctrl-H)
inoremap <c-l> <del>
" Delete line in insert mode
inoremap <c-d> <c-o>dd

" Go to previous command with Ctrl-K
cnoremap <c-k> <up>


" Close and save: buffer, if >1 buffer, or file
function CloseFile()
  update
  if len(getbufinfo({'buflisted':1})) - 1
    bdelete
  else
    execute "normal! ZZ"
  endif
endfunction


" Operator pending mappings
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap an( :<c-u>normal! f(va(<cr>
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap an{ :<c-u>normal! f{va{<cr>
" }}}


" Autocommands and abbreviations {{{
augroup prettier_ft
  au!
  autocmd BufNewFile,BufRead .prettierrc set filetype=json
augroup END



" Abbreviations/keymaps

augroup filetype_js
  autocmd!
  autocmd Filetype typescript,javascript inoremap <buffer> <tab> <c-x><c-o>
  autocmd Filetype typescript,javascript nnoremap <buffer> <localleader>/ I//<esc>

  autocmd Filetype typescript,javascript iabbrev <buffer> if if ()<left>
  autocmd Filetype typescript,javascript iabbrev <buffer> ret return;<left>

  autocmd Filetype typescript,javascript " Untrain fingers
  autocmd Filetype typescript,javascript iabbrev <buffer> return NOPE
augroup END


augroup filetype_html
  autocmd!
  autocmd Filetype html,php innoremap <buffer> <localleader>/ I<!--<c-o>A--><esc>
augroup END

 
augroup filetype_vim
  autocmd!
  autocmd Filetype vim iabbrev <buffer> iab iabbrev <buffer>
  autocmd Filetype vim setlocal foldmethod=marker
augroup END
" }}}
