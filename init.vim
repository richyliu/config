call plug#begin(stdpath('data') . '/plugged')


" For status line
Plug 'vim-airline/vim-airline'
" To customize status line for PaperColor theme
Plug 'vim-airline/vim-airline-themes'
" To display marks in the sidebar
Plug 'kshenoy/vim-signature'
" Papercolor theme
Plug 'NLKNguyen/papercolor-theme'
" To display git changes in the sidebar
Plug 'airblade/vim-gitgutter'

" For changing surroundings characters
Plug 'tpope/vim-surround'
" Auto pair quotes, braces, etc.
Plug 'jiangmiao/auto-pairs'
" Smart find and replace that preserves case
Plug 'tpope/vim-abolish'
" Allow for easy commenting via 'gcc'
Plug 'tpope/vim-commentary'
" Automatically disable search highlighting
Plug 'romainl/vim-cool'
" Shows hex code color
Plug 'chrisbra/Colorizer'

" Language server
Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }
" Completion for language server
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Reason language support
Plug 'reasonml-editor/vim-reason-plus'
" Prettier support for JS/TS
Plug 'prettier/vim-prettier'
" Javascript support
Plug 'pangloss/vim-javascript'
" JSX language support
Plug 'mxw/vim-jsx'
" Elm support
Plug 'richyliu/elm-vim', { 'for': 'elm' }
" Automatically close html tags
Plug 'alvan/vim-closetag'
" Emmet for html tag shortcuts
Plug 'mattn/emmet-vim'
" Svelte language suppor
Plug 'evanleck/vim-svelte'

" Markdown support
Plug 'git@github.com:richyliu/vim-markdown'
" Markdown Plus
Plug 'git@github.com:richyliu/markdown-plus.git'
" Better bullet lists for markdown
Plug 'dkarter/bullets.vim'

" Haskell syntax
" Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

" Latex preview
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

" Snippet support
Plug 'sirver/ultisnips'
" Allow for local vim configuration
Plug 'thinca/vim-localrc'


call plug#end()

" Airline config
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#ignore_bufadd_pat = 'defx|gundo|nerd_tree|startify|tagbar|undotree|vimfiler'

" Enable deoplete
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option({
      \ 'auto_complete_delay': 200,
      \ 'smart_case': v:true,
      \ })

" Configure language servers
let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
      \ 'reason': [stdpath('config') . '/language_servers/reason-language-server'],
      \ 'javascript': ['javascript-typescript-stdio'],
      \ 'javascript.jsx': ['javascript-typescript-stdio'],
      \ 'typescript': ['javascript-typescript-stdio'],
      \ 'typescript.tsx': ['javascript-typescript-stdio'],
      \ }

" Prettier config
let g:prettier#autoformat = 0
let g:prettier#config#parser = 'babylon'
let g:prettier#exec_cmd_path= "~/.nvm/versions/node/v12.16.1/bin/prettier"
augroup prettier_ft
    au!
    autocmd BufNewFile,BufRead .prettierrc set filetype=json
augroup END

" UltiSnips config
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" Auto close tags config
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.jsx,*.tsx,*.js,*.ts'

" Enable folding in default markdown
let g:markdown_folding = 1

let g:PaperColor_Theme_Options = {
      \  'theme': {
      \    'default.dark': {
      \      'allow_bold': 1,
      \      'allow_italic': 1,
      \      'transparent_background': 1
      \    },
      \    'default.light': {
      \      'allow_bold': 1,
      \      'allow_italic': 1,
      \      'transparent_background': 0
      \    }
      \  }
      \}

" Open pdf in default viewer
let g:livepreview_previewer = 'open'

" Configure markdown bullets to only be `-`
let g:bullets_outline_levels = ['std-']

" Configure Emmet to use JSX in ReasonML files
let g:user_emmet_settings = {
      \  'reason' : {
      \    'extends' : 'jsx',
      \  },
      \}


set autoindent
set background=light
set backspace=indent,eol,start
set backup
set backupcopy=yes
set backupdir=~/.cache/nvim/backup//
set clipboard=
set completefunc=ListSnippets
set completeopt-=preview
set cursorline
set dictionary+=/usr/share/dict/words
set directory^=$HOME/.cache/nvim/swap//
set expandtab
set foldlevelstart=20
set hidden
set hlsearch
set ignorecase
set inccommand=nosplit
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
set shiftwidth=2
set shellcmdflag=-c
set noshelltemp
set showcmd
set smartcase
set softtabstop=2
set spellcapcheck=
set spellfile=~/.config/nvim/spell/.utf-8.add
set splitright
set nostartofline
set tabstop=4
set tags+=.tags
set textwidth=0
set timeoutlen=500
set title
set ttimeoutlen=10
set undodir=~/.cache/nvim/undo/
set undofile
set vb t_vb= " Disables bell sound
set wildignore+=**/node_modules/**,node_modules/
set wildignorecase
set wildmenu
set wildmode=longest:full,full

syntax on
filetype plugin on

colorscheme PaperColor

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
" cd to current file directory
nnoremap <leader>c :cd %:p:h<cr>
" change to paste mode
nnoremap <leader>a :set paste!<cr>
" toggle relative number
nnoremap <leader>l :setlocal relativenumber!<cr>
" open help search
nnoremap <leader>h :help<space>
" change settings
nnoremap <leader>u :set<space>
" run external shell command
nnoremap ! :!
" Make ex mode harder to enter on accident
nnoremap Q :echo "To enter Ex mode, type    gQ  or start vim with 'vim -e'"<cr>
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

" redraw and refresh marks display
nnoremap <c-l> :mode \| SignatureRefresh<cr>

" Git shortcuts
nnoremap <leader>ga :!git add -A; git status; printf "\nGIT ADD ALL\n"<cr>
nnoremap <leader>gs :!git status<cr>
nnoremap <leader>gd :!git diff<cr>
nnoremap <leader>g- :!git diff --cached<cr>
nnoremap <leader>gp :!git pull<cr>
nnoremap <leader>gc :!git commit -S -m ""<left>

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

" Exit tmux and kill host
nnoremap <c-q> :silent !tmux detach-client -P<cr>

" Make ctrl-u start a separate history entry
inoremap <c-u> <c-g>u<c-u>

" Open link in google chrome
nnoremap <leader>m yi(:<c-u>silent execute '!open -a "google chrome" "' . escape(@@, '%#') . '"'<cr><c-l>

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

" Language client shortcuts
nnoremap <silent> K :call LanguageClient#textDocument_definition()<cr>
nnoremap <silent> <leader>p :call LanguageClient#textDocument_formatting()<cr>
nnoremap <silent> <cr> :call LanguageClient#textDocument_hover()<cr>
nnoremap <silent> <f1> :call LanguageClient#textDocument_rename()<cr>
nnoremap <silent> go :call LanguageClient#textDocument_documentSymbol()<cr>

" source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
    \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
    \gvy/<C-R><C-R>=escape(@", '/\.*$^~[')<CR><CR>
    \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
    \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
    \gvy?<C-R><C-R>=escape(@", '?\.*$^~[')<CR><CR>
    \gV:call setreg('"', old_reg, old_regtype)<CR>


" Make the `tags` file
command! MakeTags !ctags -f .tags -R .


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
    " press F5 to insert header of current date
    autocmd Filetype markdown inoremap <buffer> <F5> ##<space><c-r>=strftime("%Y-%m-%d %a")<cr><cr>
    " toggle spell
    autocmd Filetype markdown nnoremap <buffer> <localleader>s :set spell!<cr>
    " make a link around the word and insert into the link title
    autocmd Filetype markdown nnoremap <buffer> <localleader>l :execute "normal! i(\eEa)\eBi[]\e"<cr>:startinsert<cr>
    " disable autocomplete
    autocmd Filetype markdown call deoplete#custom#buffer_option('auto_complete', v:false)
    " disable the indentation mappings that come with bullet.vim
    autocmd BufReadPost *.md vunmap <buffer> >
    autocmd BufReadPost *.md vunmap <buffer> <
    autocmd BufReadPost *.md nunmap <buffer> >>
    autocmd BufReadPost *.md nunmap <buffer> <<
augroup END

augroup filetype_tex
    " wrap text at 80 column
    autocmd Filetype tex setlocal textwidth=80
    autocmd Filetype tex setlocal colorcolumn=80
    " toggle spell
    autocmd Filetype tex nnoremap <buffer> <localleader>s :set spell!<cr>
    " enable spell by default
    autocmd Filetype tex setlocal spell
    " disable autocomplete
    autocmd Filetype tex call deoplete#custom#buffer_option('auto_complete', v:false)
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
    autocmd Filetype javascript,typescript,jsx,tsx nnoremap <buffer> <leader>p :call <SID>CustomPrettier()<cr>
    autocmd Filetype javascript,typescript,jsx,tsx setlocal tabstop=2
    " replace class with className (for React)
    autocmd Filetype javascript,typescript,jsx,tsx nnoremap <buffer> <localleader>c :%s/class=/className=/ge<cr>:%s/fill-rule/fillRule/ge<cr>:%s/clip-rule/clipRule/ge<cr>
augroup END

" ReasonML
augroup filetype_re
    autocmd!
    " ignore compiled js files
    autocmd Filetype reason set wildignore+=*.bs.js
augroup END

" " Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff
" Source: https://vim.fandom.com/wiki/Encryption
" Modified slightly
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
    %substitute/\v +(--|-\@=[a-zA-Z]|\|)/ \\\r      \1/ge
endfunction
" }}}

" }}}

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
    execute ':vsplit ' . stdpath('config') . '/UltiSnips/'. ft . '.snippets'
endfunction

" Custom wrapper around the Prettier function so that marks are preserved
function! s:CustomPrettier()
    " let command line height be really tall to avoid hit-enter prompt
    let old_height=&cmdheight
    set cmdheight=2

    " call Prettier to format the file
    Prettier
    " undo to get the marks back
    undo

    " save the current posistion
    let save_pos = getpos('.')
    " redo, while preserving the marks
    lockmarks redo
    " restore the cursor position
    call setpos('.', save_pos)

    " restore command line height
    let &cmdheight=old_height
endfunction
