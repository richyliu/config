set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'kshenoy/vim-signature'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-commentary'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'majutsushi/tagbar'


Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-surround'
Plugin 'prettier/vim-prettier'

Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'posva/vim-vue'
Plugin 'mxw/vim-jsx'
Plugin 'plasticboy/vim-markdown'
Plugin 'valloric/matchtagalways'

Plugin 'quramy/tsuquyomi'
Plugin 'leafgarland/typescript-vim'
Plugin 'peitalin/vim-jsx-typescript'

Plugin 'Shougo/vimproc.vim'
Plugin 'idanarye/vim-vebugger'
Plugin 'elmcast/elm-vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" Plugin options {{{
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

let g:prettier#autoformat = 0
let g:prettier#config#parser = 'typescript'
" autocmd BufWritePre *.ts,*.tsx PrettierAsync

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

augroup prettier_ft
  au!
  autocmd BufNewFile,BufRead .prettierrc set filetype=json
augroup END

let g:closetag_filenames = "*.html,*.php,*.tsx"

" Enabled filetypes for html tag highlighting
let g:mta_filetypes = { 'html' : 1, 'php' : 1, 'typescript' : 1, 'javascript' : 1 }

" Close preview window for YCMD after leave insert mode
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_confirm_extra_conf = 0

" Typescript suffix for making tags (for ftplugin/typescript.vim)
let makeElementSuf = '\\'

" Tagbar setting for Elm
let g:tagbar_type_elm = {
  \ 'ctagstype': 'elm',
  \ 'kinds': [
    \ 'a:alias',
    \ 'c:constructor',
    \ 'f:function',
    \ 'm:module',
    \ 'n:namespace:1:0',
    \ 'p:port',
    \ 't:type',
  \ ],
  \ 'sort': '0'
\ }

" Tagbar time to update current tag
set updatetime=750
" }}}


" Settings {{{
set autoindent
set backspace=indent,eol,start
set backupcopy=yes
set clipboard=unnamed
set completefunc=ListSnippets
set cursorline
set expandtab
set hidden
set hlsearch
set ignorecase
set incsearch
set mouse=a
set number
set path+=**
set relativenumber
set ruler
set scrolloff=5
set shiftwidth=2
set showcmd
set smartcase
set softtabstop=2
set splitright
set tabstop=4
set tags+=.tags
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=0
set wildmenu
set wildmode=longest:full,full

syntax on
filetype plugin on

colorscheme PaperColor
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
" Open snippets
nnoremap <leader>es :vsplit $HOME/.vim/UltiSnips<cr>
" Format code
nnoremap <leader>f gg=G
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
" toggle tagbar
nnoremap <leader>t :TagbarToggle<cr>

" No need for shift to type commands
nnoremap ; :
vnoremap ; :
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
" Or next command with Ctrl-J
cnoremap <c-j> <down>
" Delete with alt-h
cnoremap ˙ <del>

" Make ctrl-c work correctly in insert and visual mode
inoremap <c-c> <esc>
vnoremap <c-c> <esc>

" Close and save: buffer, if >1 buffer, or file
function CloseFile()
  update
  if len(getbufinfo({'buflisted':1})) - 1
    bdelete
  else
    execute "normal! ZZ"
  endif
endfunction


" Make the `tags` file
command! MAKETAGS !ctags -f .tags -R .


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

augroup filetype_elm
  autocmd!
  autocmd Filetype elm setlocal tabstop=4
  autocmd Filetype elm setlocal softtabstop=4
  autocmd Filetype elm setlocal shiftwidth=4
  autocmd Filetype elm setlocal expandtab
augroup END

augroup filetype_snippets
  autocmd!
  autocmd Filetype snippets setlocal expandtab
augroup END

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
  redraw!

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

" UltiSnips completion with <C-x C-u> {{{
function! ListSnippets(findstart, base) abort
  if empty(UltiSnips#SnippetsInCurrentScope(1))
    return ''
  endif

  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && (line[start - 1] =~ '\a')
      let start -= 1
    endwhile
    return start
  else
    " find classes matching "a:base"
    let res = []
    for m in keys(g:current_ulti_dict_info)
      if m =~ a:base
        let n = {
              \ 'word': m,
              \ 'menu': '[snip] '. g:current_ulti_dict_info[m]['description']
              \ }
        call add(res, n)
      endif
    endfor
    return res
  endif
endfunction
" }}}

" }}}
