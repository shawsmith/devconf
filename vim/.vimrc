set langmenu=en_US.UTF-8
let $LANG='en'

let mapleader=" "
set nocompatible
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
set nobackup
set nowritebackup
set noswapfile
set mouse=a
set encoding=utf-8
let &t_ut=''
set noerrorbells
set vb t_vb=
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
au BufReadPost * if line("''\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
syntax on
set number
" set relativenumber
" set cursorline
hi CursorLine term=NONE cterm=NONE guibg=Grey40
set wrap
set showcmd
set wildmenu
set scrolloff=5
set tw=0
set indentexpr=
set backspace=indent,eol,start
set foldmethod=indent
set foldlevel=99
set hlsearch
exec "nohlsearch"
set incsearch
set ignorecase
set smartcase
noremap <ESC> :noh<RETURN><ESC>

map S :w<CR>
map Q :q<CR>
map R :source $MYVIMRC<CR>
map lc :e $MYVIMRC<CR>

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
map tf :tabnext<CR>
map tb :-tabnext<CR>
map tc :tabclose<CR>

" buffer
map bn :bnext<CR>
map bp :bprevious<CR>
map bd :bdelete<CR>
map bl :ls<CR>

set hidden
set nowritebackup
set autoread
au CursorHold * checktime
autocmd BufEnter * set updatetime=300

colorscheme industry

