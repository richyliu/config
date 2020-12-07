call plug#begin(stdpath('data') . '/plugged')

Plug 'vim-airline/vim-airline'        " For status line
Plug 'NLKNguyen/papercolor-theme'     " PaperColor Theme
Plug 'vim-airline/vim-airline-themes' " To customize status line for PaperColor theme
Plug 'kshenoy/vim-signature'          " To display marks in the sidebar
Plug 'airblade/vim-gitgutter'         " To display git changes in the sidebar
Plug 'ap/vim-buftabline'              " Support for displaying buffers as tabs
Plug 'rickhowe/diffchar.vim'          " Character wise diff

Plug 'sirver/ultisnips'               " Snippet support
Plug 'honza/vim-snippets'             " Provides the snippets

Plug 'tpope/vim-surround'             " For changing surroundings characters
Plug 'tpope/vim-repeat'               " Allow for repeating vim-surround
Plug 'jiangmiao/auto-pairs'           " Auto pair quotes, braces, etc.
Plug 'tpope/vim-abolish'              " Smart find and replace that preserves case
Plug 'tpope/vim-commentary'           " Allow for easy commenting via 'gcc'
Plug 'romainl/vim-cool'               " Automatically disable search highlighting
Plug 'AndrewRadev/sideways.vim'       " Easily swap arguments

Plug 'prettier/vim-prettier'          " Prettier support for JS/TS
Plug 'alvan/vim-closetag'             " Automatically close html tags
Plug 'rust-lang/rust.vim'             " Rust language support
Plug 'racer-rust/vim-racer'           " Racer rust autocomplete

call plug#end()

" Show modified files in buffer tabline
let g:buftabline_indicators=1

" Autoformat rust files on save
let g:rustfmt_autosave=1

" PaperColor theme
set t_Co=256
set background=light
colorscheme PaperColor

" Settings
set autoindent
set background=light
set backspace=indent,eol,start
set backup
set backupcopy=yes
set backupdir=~/.cache/nvim/backup//
set clipboard=
set completefunc=ListSnippets
set cursorline
set directory^=$HOME/.cache/nvim/swap//
set expandtab
set hidden
set hlsearch
set ignorecase
set incsearch
set inccommand=nosplit
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
set shiftwidth=2
set noshelltemp
set showcmd
set smartcase
set softtabstop=2
set spellcapcheck=
set splitright
set nostartofline
set tabstop=4
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=10
set undodir=~/.cache/nvim/undo/
set undofile
set updatetime=500
set vb t_vb= " Disables bell sound
set wildignore+=**/node_modules/**
set wildignorecase
set wildmenu
set wildmode=longest:full,full

" Keymaps and Abbrev
let mapleader = " "
let maplocalleader = "Q"
" Save file
noremap <leader>w :w<cr>
" Source current file
noremap <leader>o :source %<cr>
" Run the previous command
noremap <leader>r :!!<cr>
" Open vimrc file
nnoremap <leader>ev :e $MYVIMRC<cr>
" Open snippets
nnoremap <leader>es :call <SID>OpenSnippetFile()<cr>
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
nnoremap _ :.m.-2<cr>
nnoremap + :.m+<cr>
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

" Use custom gitgutter mappings
let g:gitgutter_map_keys = 0
nmap <leader>ghp <Plug>(GitGutterPreviewHunk)
nmap <leader>ghs <Plug>(GitGutterStageHunk)
nmap <leader>ghu <Plug>(GitGutterUndoHunk)
" Default gitgutter mappings
nmap [c <Plug>(GitGutterPrevHunk)
nmap ]c <Plug>(GitGutterNextHunk)
omap ic <Plug>(GitGutterTextObjectInnerPending)
omap ac <Plug>(GitGutterTextObjectOuterPending)
xmap ic <Plug>(GitGutterTextObjectInnerVisual)
xmap ac <Plug>(GitGutterTextObjectOuterVisual)

" Move argument left/right
nnoremap <c-h> :<c-u>SidewaysLeft<cr>
nnoremap <c-l> :<c-u>SidewaysRight<cr>

" Search for selected text and automatically escape characters
" Source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=escape(@", '/\.*$^~[')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=escape(@", '?\.*$^~[')<CR><CR>
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


augroup filetype_vim
  autocmd!
  autocmd Filetype vim iabbrev <buffer> iab iabbrev <buffer>
  autocmd Filetype vim setlocal foldmethod=marker
augroup END

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
  " start bold text
  autocmd Filetype markdown inoremap <buffer> <c-b> ****<left><left>
  autocmd Filetype markdown iabbrev <buffer> --> →
  " need to allow recursive map to make surround work
  autocmd Filetype markdown vmap <buffer> <c-b> S*gvS*
  autocmd Filetype markdown setlocal complete=kspell
  " wrap text at 80 column
  autocmd Filetype markdown setlocal textwidth=80
  autocmd Filetype markdown setlocal colorcolumn=80
  " enable spell by default
  autocmd Filetype markdown setlocal spell
  autocmd Filetype markdown setlocal spellcapcheck=
  " press F5 to insert header of current date
  autocmd Filetype markdown inoremap <buffer> <F5> ##<space><c-r>=strftime("%Y-%m-%d %a")<cr><cr>
  " toggle spell
  autocmd Filetype markdown nnoremap <buffer> <localleader>s :set spell!<cr>
  " make a link around the word and insert into the link title
  autocmd Filetype markdown nnoremap <buffer> <localleader>l :execute "normal! i(\eEa)\eBi[]\e"<cr>:startinsert<cr>
augroup END

augroup filetype_tex
  " wrap text at 80 column
  autocmd Filetype tex setlocal textwidth=80
  autocmd Filetype tex setlocal colorcolumn=80
  " toggle spell
  autocmd Filetype tex nnoremap <buffer> <localleader>s :set spell!<cr>
  " enable spell by default
  autocmd Filetype tex setlocal spell
augroup END

augroup filetype_sh
  autocmd!
  autocmd Filetype sh nnoremap <buffer> <localleader>c "+ci'
  autocmd Filetype sh nnoremap <buffer> <localleader>p :put +<cr>
  " Format shell command
  autocmd Filetype sh nnoremap <buffer> <F3> :call <SID>FormatCommand()<cr>
augroup END

augroup filetype_zsh
  " Format shell command
  autocmd Filetype zsh nnoremap <buffer> <F3> :call <SID>FormatCommand()<cr>
augroup END

" Javascript, Typescript, and TSX
augroup filetype_js
  autocmd!
  " invoke prettier to format document
  autocmd Filetype javascript,typescript,jsx,tsx nnoremap <buffer> <localleader>p :Prettier<cr>
  autocmd Filetype javascript,typescript,jsx,tsx setlocal tabstop=2
  " replace class with className (for React)
  autocmd Filetype javascript,typescript,jsx,tsx nnoremap <buffer> <localleader>c :%s/class=/className=/ge<cr>:%s/fill-rule/fillRule/ge<cr>:%s/clip-rule/clipRule/ge<cr>
augroup END

augroup filetype_rs
  autocmd!
  autocmd Filetype rust nnoremap <buffer> <localleader>p :RustFmt<cr>
augroup END

augroup Racer
    autocmd!
    autocmd FileType rust nmap <buffer> gd         <Plug>(rust-def)
    autocmd FileType rust nmap <buffer> gs         <Plug>(rust-def-split)
    autocmd FileType rust nmap <buffer> gx         <Plug>(rust-def-vertical)
    autocmd FileType rust nmap <buffer> gt         <Plug>(rust-def-tab)
    autocmd FileType rust nmap <buffer> <localleader>gd <Plug>(rust-doc)
    autocmd FileType rust nmap <buffer> <localleader>gD <Plug>(rust-doc-tab)
augroup END

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
  autocmd BufReadPre,FileReadPre *.gpg set bin
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


" Formats shell commands by intersting a backslack and a newline before
" arugments (dashes) and pipes (vertical bars)
function! s:FormatCommand()
  %substitute/\v +(--|-\@=[a-zA-Z]|\|)/ \\\r    \1/ge
endfunction

" UltiSnips completion with <C-x C-u>
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

" Opens the snippet file for the current filetype in a new split
" If there are multiple file types, the first one is used
function! s:OpenSnippetFile()
  let ft = substitute(&filetype, '^\([^.]\+\)\..\+$', '\1', '')
  execute ':vsplit ' . stdpath('data') . '/UltiSnips/' . ft . '.snippets'
endfunction
