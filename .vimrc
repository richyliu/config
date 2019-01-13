set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'kshenoy/vim-signature'
Plugin 'scrooloose/nerdtree'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-commentary'
" Plugin 'mhinz/vim-startify'
" Plugin 'thaerkh/vim-workspace'

Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-surround'
Plugin 'prettier/vim-prettier'
Plugin 'valloric/youcompleteme'
" Plugin 'vim-syntastic/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
" Plugin 'alvan/vim-closetag'

Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'posva/vim-vue'
Plugin 'mxw/vim-jsx'
Plugin 'plasticboy/vim-markdown'
Plugin 'valloric/matchtagalways'

Plugin 'quramy/tsuquyomi'
Plugin 'leafgarland/typescript-vim'
Plugin 'peitalin/vim-jsx-typescript'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" Plugin options {{{
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

let g:prettier#autoformat = 0
let g:prettier#config#parser = 'typescript'
autocmd BufWritePre *.ts,*.tsx PrettierAsync

let g:monokai_term_italic = 1
let g:monokai_gui_italic = 0

" Expand snips on ctrl-space (sends ^J)
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-t>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

augroup tsu_custom_opt
  autocmd!
  autocmd FileType typescript nnoremap <buffer> <leader>t : <c-u>echo tsuquyomi#hint()<cr>
augroup END

let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_by_filename = 1

" make sure relative line numbers are used for NERD tree
autocmd FileType nerdtree setlocal relativenumber

augroup prettier_ft
  au!
  autocmd BufNewFile,BufRead .prettierrc set filetype=json
augroup END

let g:closetag_filenames = "*.html,*.php,*.tsx"

" Enabled filetypes for html tag highlighting
let g:mta_filetypes = { 'html' : 1, 'php' : 1, 'typescript' : 1, 'javascript' : 1 }

" Close preview window for YCMD after leave insert mode
let g:ycm_autoclose_preview_window_after_completion = 1
" }}}


" Settings {{{
set autoindent
set backspace=indent,eol,start
set clipboard=unnamed
set cursorline
set expandtab
set hidden
set hlsearch
set ignorecase
set incsearch
set mouse=a
set number
set relativenumber
set ruler
set scrolloff=5
set shiftwidth=2
set showcmd
set smartcase
set softtabstop=2
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=0
set wildmenu

syntax on
filetype plugin on

colorscheme monokai
" }}}


" Keymaps and Abbrev {{{
let mapleader = ","
let maplocalleader = "L"
" Save all file
noremap <leader>s :wall<cr>
" Source current file
noremap <leader>so :source %<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open vimrc file on a adjacent window
nnoremap <leader>ev :e $MYVIMRC<cr>
" Open typescript snippets
nnoremap <leader>es :vsplit $HOME/.vim/UltiSnips/typescript.snippets<cr>
" Format code
nnoremap <leader>f gg=G
" Save and close file
noremap <leader>z :call CloseFile()<cr>
noremap <leader>za :xa<cr>
" Navigate split window
nnoremap <leader>wl <c-w>l
nnoremap <leader>wh <c-w>h
nnoremap <leader>wj <c-w>j
nnoremap <leader>wk <c-w>k
" Nagivate windows
nnoremap <leader>wo :only<cr>
nnoremap <leader>ww <c-w>w
nnoremap <leader>wq <c-w>q
" Stop highlighting search
nnoremap <leader>nh :nohlsearch<cr>
" Open nerdtree
nnoremap <leader>n :NERDTreeToggle<cr>
" Manage sessions
let g:sessions_dir = '~/.vim/vim-sessions/'
exec 'nnoremap <Leader>ss :mksession! ' . g:sessions_dir . '<c-d>'
exec 'nnoremap <Leader>sl :source ' . g:sessions_dir. '<c-d>'
" Delete and change without modifying current register
nnoremap <leader>d "_d
nnoremap <leader>c "_c

" No need for shift to type commands
nnoremap ; :
" Move line forward or backward
nnoremap _ ddkP
nnoremap + ddp
" Cycle through buffers
nnoremap <tab> :bnext<cr>
nnoremap <s-tab> :bprevious<cr>
" Go down a page
nnoremap f <c-d>
vnoremap f <c-d>
" Yank until end of line
nnoremap Y y$

" Delete with Ctrl-L (backspace w/ Ctrl-H)
inoremap <c-l> <del>
" Delete line in insert mode
inoremap <c-d> <c-o>dd
" Go to next line on ctrl-enter (^J code)
"inoremap <c-j> <esc>o

" Go to previous command with Ctrl-K
cnoremap <c-k> <up>
nnoremap <c-k> :<up>
" Delete with alt-h
cnoremap ˙ <del>


" Close and save: buffer, if >1 buffer, or file
function CloseFile()
  update
  if len(getbufinfo({'buflisted':1})) - 1
    " Ensures window stays
    bprevious | bdelete #
  else
    execute "normal! ZZ"
  endif
endfunction


" Operator pending mappings
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap an( :<c-u>normal! f(va(<cr>
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap an{ :<c-u>normal! f{va{<cr>
onoremap in' :<c-u>normal! f'vi'<cr>
onoremap an' :<c-u>normal! f'va'<cr>
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap an" :<c-u>normal! f"va"<cr>
" }}}


" Autocommands and abbreviations {{{

" Abbreviations/keymaps {{{
augroup filetype_vim
  autocmd!
  autocmd Filetype vim nnoremap <buffer> <localleader>c I"<esc>
  autocmd Filetype vim iabbrev <buffer> iab iabbrev <buffer>
  autocmd Filetype vim setlocal foldmethod=marker
augroup END
" }}}

" }}}


" Custom Plugins {{{

" Grep operator {{{
nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@
vnoremap <leader>g :<c-u>call GrepOperator(visualmode())<cr>

function! s:GrepOperator(type)
  let saved_unnamed_register = @@

  if a:type ==# 'v'
    normal! `<v`>y
  elseif a:type ==# 'char'
    normal! `[y`]
  else
    return
  endif

  silent execute "grep! -R " . shellescape(@@) . " ."
  copen

  let @@ = saved_unnamed_register
endfunction
" }}}

" Options togglers {{{

" Fold column {{{
" nnoremap <leader>f :call <SID>FoldColumnToggle()<cr>

" function! s:FoldColumnToggle()
"   if &foldcolumn
"     setlocal foldcolumn=0
"   else
"     setlocal foldcolumn=4
"   endif
" endfunction
" }}}

" Relative line number {{{
nnoremap <leader>l :call <SID>RelativeNumberToggle()<cr>

function! s:RelativeNumberToggle()
  if &relativenumber
    setlocal norelativenumber
  else
    setlocal relativenumber
  endif
endfunction
" }}}

" Quick fix {{{
nnoremap <leader>q :call <SID>QuickfixToggle()<cr>

let g:quickfix_is_open = 0

function! s:QuickfixToggle()
  if g:quickfix_is_open
    cclose
    let g:quickfix_is_open = 0
    execute g:quickfix_return_to_window . "wincmd w"
  else
    let g:quickfix_return_to_window = winnr()
    copen
    let g:quickfix_is_open = 1
  endif
endfunction
" }}}

" }}}

" }}}
