call plug#begin(stdpath('data') . '/plugged')

Plug 'itchyny/lightline.vim'          " Lightline status line
Plug 'chriskempson/base16-vim'        " Base16 color scheme
Plug 'kshenoy/vim-signature'          " To display marks in the sidebar
Plug 'airblade/vim-gitgutter'         " To display git changes in the sidebar
Plug 'ap/vim-buftabline'              " Support for displaying buffers as tabs
Plug 'rickhowe/diffchar.vim'          " Character wise diff
Plug 'junegunn/fzf.vim'               " Fuzzy finder
Plug '/opt/homebrew/opt/fzf'          " Needed for fzf vim plugin (M1 homebrew)

Plug 'sirver/ultisnips'               " Snippet support
Plug 'honza/vim-snippets'             " Provides the snippets

Plug 'tpope/vim-surround'             " For changing surroundings characters
Plug 'tpope/vim-repeat'               " Allow for repeating vim-surround
Plug 'Raimondi/delimitMate'           " Auto pair quotes, braces, etc.
Plug 'tpope/vim-abolish'              " Smart find and replace that preserves case
Plug 'tpope/vim-commentary'           " Allow for easy commenting via 'gcc'
Plug 'romainl/vim-cool'               " Automatically disable search highlighting
Plug 'AndrewRadev/sideways.vim'       " Easily swap arguments

Plug 'pangloss/vim-javascript'        " JS syntax
Plug 'MaxMEllon/vim-jsx-pretty'       " JSX syntax
Plug 'prettier/vim-prettier'          " Prettier (JS formatter)
Plug 'mattn/emmet-vim'                " Emmet
Plug 'alvan/vim-closetag'             " Automatically close html tags
Plug 'rust-lang/rust.vim'             " Rust language support
Plug 'autozimu/LanguageClient-neovim' " Language server client
Plug 'cespare/vim-toml'               " TOML config file format support
Plug 'glench/vim-jinja2-syntax'       " Jinja2 template (closest to Tera) support
Plug 'purescript-contrib/purescript-vim' " Purescript language suppot
Plug 'evanleck/vim-svelte'            " Svelte language support
Plug 'metakirby5/codi.vim'            " Python scratchpad

call plug#end()

" Lightline customize theme for one-light

" Common colors
let s:blue   = [ '#4078f2', 75 ]
let s:orange = [ '#d75f00', 166 ]
let s:red1   = [ '#e06c75', 168 ]
let s:red2   = [ '#be5046', 168 ]
let s:yellow = [ '#e5c07b', 180 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

" Light variant
let s:fg    = [ '#494b53', 238 ]
let s:bg    = [ '#fafafa', 255 ]
let s:gray1 = [ '#494b53', 238 ]
let s:gray2 = [ '#f0f0f0', 255 ]
let s:gray3 = [ '#d0d0d0', 250 ]
let s:green = [ '#50a14f', 35 ]

let s:p.inactive.left   = [ [ s:bg,  s:gray3 ], [ s:bg, s:gray3 ] ]
let s:p.inactive.middle = [ [ s:gray3, s:gray2 ] ]
let s:p.inactive.right  = [ [ s:bg, s:gray3 ] ]

" Common
let s:p.normal.left    = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.normal.middle  = [ [ s:fg, s:gray2 ] ]
let s:p.normal.right   = [ [ s:bg, s:blue, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.normal.error   = [ [ s:red2, s:bg ] ]
let s:p.normal.warning = [ [ s:yellow, s:bg ] ]
let s:p.insert.right   = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.insert.left    = [ [ s:bg, s:green, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.replace.right  = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.replace.left   = [ [ s:bg, s:red1, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.visual.right   = [ [ s:bg, s:orange, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.visual.left    = [ [ s:bg, s:orange, 'bold' ], [ s:fg, s:gray3 ] ]
let s:p.tabline.left   = [ [ s:fg, s:gray3 ] ]
let s:p.tabline.tabsel = [ [ s:bg, s:orange, 'bold' ] ]
let s:p.tabline.middle = [ [ s:gray3, s:gray2 ] ]
let s:p.tabline.right  = copy(s:p.normal.right)

let g:lightline#colorscheme#one#palette = lightline#colorscheme#flatten(s:p)

" In dark mode, this command will succeed (no error)
silent !grep -q Dark <(defaults read -g AppleInterfaceStyle)
let s:is_light_mode = v:shell_error

" conditional color schemes based on light/dark mode
if s:is_light_mode
  let s:lightline_colorscheme = 'one'
  colorscheme base16-one-light
else
  let s:lightline_colorscheme = 'Tomorrow_Night'
  colorscheme base16-tomorrow-night
endif


" Lightline statusbar options
let g:lightline = {
      \ 'colorscheme': s:lightline_colorscheme,
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent', 'wordcount' ], [ 'spell', 'filetype' ] ]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'filename' ], [ 'modified' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent' ] ]
      \ },
      \ 'component_function': {
      \   'wordcount': 'WordCount',
      \ },
      \ }

function! WordCount()
  if &filetype != 'markdown' && &filetype != 'text'
    return ''
  endif
  return wordcount().words . ' words'
endfunction

" Buffer tabline settings
let g:buftabline_indicators=1
let g:buftabline_numbers=2

" Autoformat rust files on save
let g:rustfmt_autosave=1

" Configure purescript indent
let g:purescript_disable_indent = 1

" Language client settings
let g:LanguageClient_serverCommands = {
\ 'rust': {
\   'name': 'rust-analyzer',
\   'command': ['rust-analyzer'],
\   'initializationOptions': {
\     'diagnostics': {
\       'disabled': ['unresolved-proc-macro'],
\     },
\     'lens': {
\       'enable': v:true,
\     },
\   },
\ },
\ 'purescript': {
\   'name': 'purescript language server',
\   'command': ['purescript-language-server', '--stdio'],
\ }
\}
let g:LanguageClient_preferredMarkupKind = ['markdown']

" Markdown code highlighting
let g:markdown_fenced_languages = ['rust']

" Ultisnip open edit window in horizontal split
let g:UltiSnipsEditSplit="horizontal"

" Configure python3 for codi
let g:codi#interpreters = {
      \ 'python': {
        \ 'bin': 'python3'
        \ },
      \ }

" delimitMate auto cloing options
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1

" Settings
set autoindent
set backspace=indent,eol,start
set backup
set backupcopy=yes
set backupdir=~/.cache/nvim/backup//
set breakindent
set breakindentopt=min:20,sbr
set clipboard=
set completefunc=ListSnippets
set cursorline
set directory^=$HOME/.cache/nvim/swap//
set display+=truncate
set expandtab
set hidden
set history=10000
set hlsearch
set ignorecase
set incsearch
set inccommand=nosplit
set lazyredraw
set linebreak
set list
set listchars=tab:→\ ,trail:·,extends:⟩,precedes:⟨
set nomodeline
set mouse=nvc
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
set showbreak=↪
set showcmd
set smartcase
set softtabstop=2
set spellcapcheck=
set splitbelow
set splitright
set nostartofline
set tabstop=4
set termguicolors
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
set nowrapscan

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
nnoremap <leader>es :UltiSnipsEdit<cr>
" Open file in current folder
nnoremap <leader>e :e <c-d>
nnoremap <leader>ee :e <c-d>
" Format with prettier
nnoremap <leader>f :Prettier<cr>
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
" yank file to system clipboard
nnoremap <leader>yA :<c-u>%y+<cr>
" open quickfix window
nnoremap <leader>q :<c-u>copen<cr>
" move to adjacent buffers
nnoremap <leader>] :<c-u>bnext<cr>
nnoremap <leader>[ :<c-u>bprev<cr>
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
" Make shift tab go back in jumplist
nnoremap <s-tab> <c-o>
" Alternative way to exit insert mode
inoremap jj <ESC>

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
nnoremap <c-k> :<c-u>SidewaysRight<cr>

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

" Terminal-mode
tnoremap <esc> <C-\><C-n>
tnoremap <C-v><esc> <esc>
augroup terminal_mode
  autocmd!
  autocmd TermOpen * setlocal scrolloff=0
augroup END

" Highlighting overrides
augroup terminal_cursor
  autocmd!
  " Cursor color in terminal mode
  autocmd BufCreate * highlight! link TermCursor Cursor
  autocmd BufCreate * highlight! TermCursorNC guibg=black guifg=white ctermbg=1 ctermfg=15
  " Buffer shown in other window has same coloring
  autocmd BufCreate * highlight! link BufTabLineActive TabLine
augroup END

" Hacky delay to get this to run after the colorscheme
" Overrides the buffer tabline colors to be more visible
call timer_start(100, { tid -> execute('highlight! TabLineSel guibg=#fafafa guifg=#50a14f ctermbg=10 ctermfg=02') })
" Make select wild menu colors more readable
call timer_start(100, { tid -> execute('highlight! WildMenu guibg=#383a42 guifg=#f0f0f1 ctermbg=07 ctermfg=10') })
" Highlight non ascii
call timer_start(100, { tid -> execute('syntax match nonascii "[^\x00-\x7F]"') })
call timer_start(150, { tid -> execute('highlight! nonascii guibg=Red guifg=white ctermbg=5 ctermfg=15') })


" LanguageClient mappings
nmap <silent> gd <Plug>(lcn-definition)
nmap <silent> K <Plug>(lcn-hover)
nmap <silent> <F1> <Plug>(lcn-menu)
nmap <silent> <F2> <Plug>(lcn-code-action)
nmap <silent> <F3> <Plug>(lcn-code-lens-action)
nmap <silent> <F4> <Plug>(lcn-highlight)
nmap <silent> <F6> <Plug>(lcn-references)
nnoremap <silent> <F16> :<c-u>call LanguageClient_clearDocumentHighlight()<cr>

" Switch to buffer number
nmap <leader>1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>BufTabLine.Go(-1)

" Switch to buffer number with alt-number
nmap <M-1> <Plug>BufTabLine.Go(1)
nmap <M-2> <Plug>BufTabLine.Go(2)
nmap <M-3> <Plug>BufTabLine.Go(3)
nmap <M-4> <Plug>BufTabLine.Go(4)
nmap <M-5> <Plug>BufTabLine.Go(5)
nmap <M-6> <Plug>BufTabLine.Go(6)
nmap <M-7> <Plug>BufTabLine.Go(7)
nmap <M-8> <Plug>BufTabLine.Go(8)
nmap <M-9> <Plug>BufTabLine.Go(-1)

" fzf mappings
nnoremap <leader>ie :Files<cr>
nnoremap <leader>id :Files %:p:h<cr>
nnoremap <leader>ig :GFiles<cr>
nnoremap <leader>ii :GFiles<cr>
nnoremap <leader>ic :GFiles?<cr>
nnoremap <leader>ib :Buffers<cr>
nnoremap <leader>ia :Ag<cr>
nnoremap <leader>il :Lines<cr>
nnoremap <leader>is :BLines<cr>

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

augroup filetype_svelte
  " invoke prettier to format document
  autocmd Filetype svelte nnoremap <buffer> <localleader>p :Prettier<cr>
  autocmd Filetype svelte setlocal tabstop=2
  autocmd Filetype svelte UltiSnipsAddFiletypes svelte.javascript
augroup END

augroup filetype_rs
  autocmd!
  autocmd Filetype rust nnoremap <buffer> <localleader>p :RustFmt<cr>
augroup END

augroup filetype_cpp
  autocmd!
  autocmd Filetype cpp nnoremap <buffer> <localleader>p :py3file /usr/local/Cellar/clang-format/12.0.0/share/clang/clang-format.py<cr>
  autocmd Filetype cpp inoremap <buffer> <localleader>p :py3file /usr/local/Cellar/clang-format/12.0.0/share/clang/clang-format.py<cr>
augroup END

augroup shorthand_transcription
  autocmd!
  autocmd BufRead,BufNewFile ~/Documents/shorthand_practice/* set textwidth=72
augroup END

augroup lisp
  autocmd!
  " Only auto pair parentheses and quotes in lisp
  autocmd Filetype lisp let b:AutoPairs = {'(':')', '"':'"'}
augroup END

augroup purescript
  autocmd!
  autocmd Filetype purescript nmap <buffer> <localleader>p <Plug>(lcn-format)
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

" Replace fancy quotes with normal quotes
function! NormalQuotes()
  %substitute/\v(“|”)/"/ge
  %substitute/\v(‘|’)/'/ge
endfunction
