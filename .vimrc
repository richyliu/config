set nocompatible              " be iMproved, required
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'        " For status line
Plugin 'vim-airline/vim-airline-themes' " To customize status line for PaperColor theme
Plugin 'kshenoy/vim-signature'          " To display marks in the sidebar
Plugin 'airblade/vim-gitgutter'         " To display git changes in the sidebar
Plugin 'NLKNguyen/papercolor-theme'     " Theme
Plugin 'majutsushi/tagbar'              " Display tags in an outline format

Plugin 'tpope/vim-commentary'           " Allow for easy commenting via 'gcc'
Plugin 'matchit.zip'                    " Extend '%' matching ability
Plugin 'tpope/vim-abolish'              " Smart find and replace that preserves case
Plugin 'jiangmiao/auto-pairs'           " Auto pair quotes, braces, etc.
Plugin 'tpope/vim-surround'             " For changin surroundings (with 's')
Plugin 'alvan/vim-closetag'             " Automatically close html tags
Plugin 'mattn/emmet-vim'                " Emmet for html tag shortcuts

"Plugin 'neoclide/coc.nvim'              " For autocomplete and language server support
Plugin 'quramy/tsuquyomi'               " Typescript lanaguage server

Plugin 'sirver/ultisnips'               " Snippet support
Plugin 'honza/vim-snippets'             " Provides the snippets

Plugin 'prettier/vim-prettier'          " Prettier support for JS/TS
Plugin 'mxw/vim-jsx'                    " JSX language support
Plugin 'leafgarland/typescript-vim'     " General typescript support
Plugin 'peitalin/vim-jsx-typescript'    " TSX support
Plugin 'posva/vim-vue'                  " Vue language support
Plugin 'gabrielelana/vim-markdown'      " Markdown support
Plugin 'hail2u/vim-css3-syntax'         " CSS3 language support
Plugin 'ap/vim-css-color'               " Highlights CSS color variables with color
Plugin 'richyliu/elm-vim'               " Elm support

Plugin 'Shougo/vimproc.vim'             " Required for vebugger
Plugin 'idanarye/vim-vebugger'          " C/C++ debugger

Plugin 'neovimhaskell/haskell-vim'      " Haskell syntax

Plugin 'lervag/vimtex'                  " Basic Latex syntax
Plugin 'xuhdev/vim-latex-live-preview'  " Latex preview

Plugin 'thinca/vim-localrc'             " Allow for local vim configuration


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

" Tagbar setting for typescript
let g:tagbar_type_typescript = {
  \ 'ctagstype' : 'typescript',
  \ 'kinds': [
    \ 'a:abstractclasses:0:1',
    \ 'e:enums:0:1',
    \ 'f:function:0:1',
    \ 't:types:0:1',
    \ 'n:modules:0:1',
    \ 'i:interface:0:1',
    \ 'c:classes:0:1',
    \ 'l:varlambda:0:1',
    \ 'v:variable:0:1',
    \ 'm:members:0:1',
  \ ]
\ }
" Tagbar time to update current tag
set updatetime=750
" Tagbar show relative line numbers
let g:tagbar_show_linenumbers=2

" Disable gitgutter mappings
let g:gitgutter_map_keys = 0

" Disable vim-markdown default mapping
let g:markdown_enable_mappings = 0
" }}}


" Settings {{{
set autoindent
set background=dark
set backspace=indent,eol,start
set backupcopy=yes
set clipboard=unnamed
set completefunc=ListSnippets
set cursorline
set dictionary+=/usr/share/dict/words
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
set tags+=.tags
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=10
set wildignore+=**/node_modules/**
set wildignorecase
set wildmenu
set wildmode=longest:full,full

" Correct function keys (for urxvt on Ubuntu 16)
set <f1>=[11~
set <f2>=[12~
set <f3>=[13~
set <f4>=[14~
set <f5>=[15~
set <f6>=[17~
set <f7>=[18~
set <f8>=[19~
set <f9>=[20~
set <f10>=[21~
set <f11>=[23~
set <f12>=[24~

syntax on
filetype plugin on

colorscheme PaperColor

" Prevent weird mouse bugs on urxvt
call timer_start(500, { tid -> execute('set ttymouse=xterm') })
" }}}


" Keymaps and Abbrev {{{
let mapleader = " "
let maplocalleader = "Q"
" Save all file
noremap <leader>s :w<cr>
" Source current file
noremap <leader>o :source %<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open vimrc file on a adjacent window
nnoremap <leader>ev :e $MYVIMRC<cr>
" Open snippets
nnoremap <leader>es :vsplit $HOME/.vim/UltiSnips<cr>
" Open file in current folder
nnoremap <leader>e :e <c-d>
" Open file
nnoremap <leader>f :find 
" Save and close file
nnoremap <leader>z :call CloseFile()<cr>
" Split windows
nnoremap <leader>ws :split<cr>
nnoremap <leader>wv :vsplit<cr>
" Stop highlighting search
nnoremap <leader>n :nohlsearch<cr>
" cd to current file directory
nnoremap <leader>c :cd %:p:h<cr>
" toggle tagbar
nnoremap <leader>t :TagbarToggle<cr>
" source vimrc (to fix mouse bug on tmux)
nnoremap <leader>m :source $MYVIMRC<cr>
" yank to system clipboard
nnoremap <leader>y "+y
" change to paste mode
nnoremap <leader>a :set paste!<cr>
" invoke prettier to format document
nnoremap <leader>p :Prettier<cr>
" open help search
nnoremap <leader>h :help 
" find and replace
nnoremap <leader>% :%s/
vnoremap <leader>% :s/
" change settings
nnoremap <leader>u :set 
" run external shell command
nnoremap ! :!
" quit all files
nnoremap <leader>q :qa<cr>
" Make ex mode harder to enter on accident
nnoremap Q :echo "To enter Ex mode, type  gQ  or start vim with 'vim -e'"<cr>
" switch buffers
nnoremap <leader>b :ls<cr>:b 

" Git shortcuts
nnoremap <leader>ga :!git add -A; git status; printf "\nGIT ADDED ALL\n"<cr>
nnoremap <leader>gs :!git status<cr>
nnoremap <leader>gd :!git diff<cr>
nnoremap <leader>g- :!git diff --cached<cr>
nnoremap <leader>gp :!git pull<cr>
nnoremap <leader>gc :!git commit -S -m ""<left>

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


" Make the `tags` file
command! MakeTags !ctags -f .tags -R .


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

augroup filetype_haskell
  autocmd!
  autocmd Filetype haskell setlocal formatprg=hindent
  autocmd Filetype haskell nnoremap <buffer> <localleader>r :!stack ghci %<cr>
augroup END

augroup filetype_markdown
  autocmd!
  autocmd Filetype markdown inoremap <buffer> <c-b> ****<left><left>
  autocmd Filetype markdown inoremap <buffer> --> →
  " Need to allow recursive map to make surround work
  autocmd Filetype markdown vmap <buffer> <c-b> S*gvS*
  autocmd Filetype markdown setlocal complete=kspell
  autocmd Filetype markdown inoremap <buffer> <F3> #<space><c-r>=strftime("%Y-%m-%d %a")<cr><cr>
augroup END

" }}}


" Custom Plugins {{{

" Grep operator {{{
" nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@
" vnoremap <leader>g :<c-u>call <SID>GrepOperator(visualmode())<cr>

function! s:GrepOperator(type)
  let saved_unnamed_register = @@

  if a:type ==# 'v'
    normal! `<v`>y
  elseif a:type ==# 'char'
    normal! `[y`]
  else
    return
  endif

  try
    execute 'vimgrep /'. escape(@@, '/\.*$^~[') . '/g **'
    copen
  endtry

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
" nnoremap <leader>q :call <SID>QuickfixToggle()<cr>

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

" Toggle background between light and dark

nnoremap <f4> :call <SID>ToggleBackground()<cr>
function! s:ToggleBackground()
  if &background == "dark"
    let &background = "light"
  else
    let &background = "dark"
  endif
endfunction

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
