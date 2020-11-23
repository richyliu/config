" Vimrc for IntelliJ Vim plugin
" Store as ~/.ideavimrc

set relativenumber
set number
set hlsearch
set incsearch
set ignorecase
set smartcase
set surround
set commentary

nnoremap <space>w :<c-u>w<cr>
nnoremap <space>% :%s/
vnoremap <space>% :s/
nnoremap _ :.m.-2<cr>
nnoremap + :.m+<cr>

nnoremap D d$
nnoremap Y y$
