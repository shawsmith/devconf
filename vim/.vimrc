let mapleader=" "
set nocompatible
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
set mouse=a
set encoding=utf-8
let &t_ut=''
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
au BufReadPost * if line("''\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
syntax on
set number
set relativenumber
set cursorline
set wrap
set showcmd
set wildmenu
set scrolloff=5
set hlsearch
exec "nohlsearch"
set incsearch
set ignorecase
set smartcase
noremap <ESC> :noh<RETURN><ESC>

map s :<nop>
map S :w<CR>
map Q :q<CR>
map R :source $MYVIMRC<CR>

" split window
map sh :set nosplitright<CR>:vsplit<CR>
map sl :set splitright<CR>:vsplit<CR>
map sj :set splitbelow<CR>:split<CR>
map sk :set nosplitbelow<CR>:split<CR>

" switch window
map <LEADER>h <C-w>h
map <LEADER>l <C-w>l
map <LEADER>j <C-w>j
map <LEADER>k <C-w>k
map <LEADER>w <C-w>w

" resize window
map <up> :res +5<CR>
map <down> :res -5<CR>
map <left> :vertical resize -5<CR>
map <right> :vertical resize +5<CR>

" tab
map tn :tabe<CR>
map tl :tabnext<CR>
map th :-tabnext<CR>
map tc :tabclose<CR>

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'

call plug#end()

" set airline theme
let g:airline_theme='ayu_mirage'

" nerd tree"
noremap tt :NERDTreeToggle<CR>

