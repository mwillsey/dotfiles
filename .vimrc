" my vimrc

""""""""""
" Vundle "
""""""""""

set nocompatible
filetype off                

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/vundle'

Plugin 'altercation/vim-colors-solarized'
Plugin 'junegunn/vim-easy-align'
Plugin 'tpope/vim-commentary'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'dag/vim2hs'

call vundle#end() 
filetype plugin indent on 

""""""
" UI "
""""""

" basic stuff
set autoread title encoding=utf-8 showcmd mouse=a ruler laststatus=2 ttyfast

" ignore whitespace in diff mode
set diffopt+=iwhite

" Be able to arrow key and backspace across newlines
set whichwrap=bs<>[]

" Make backspace not terrible
set backspace=indent,eol,start

" remember last cursor position
autocmd BufReadPost *
	\ if line("'\"") > 0 && line("'\"") <= line("$") |
	\ 	exe "normal g`\"" |
	\ endif

" proper word wrapping
set wrap linebreak

" enable completion
set omnifunc=syntaxcomplete#Complete

"""""""""""""
" Shortcuts "
"""""""""""""

" make Y behave like D (go to end of line only)
nnoremap Y y$

" for faster motion
nnoremap <C-j> <C-d>
nnoremap <C-k> <C-u>

" emacs-like spell correction with <c-l> that puts you back in insert
imap <C-l> <C-g>u<Esc>[s1z=`]A<C-g>u

"""""""""""
" Plugins "
"""""""""""

" easy-align in visual mode
vmap <CR> <Plug>(EasyAlign)

"""""""""""""
" Searching "
"""""""""""""

set hlsearch incsearch ignorecase smartcase gdefault 

" enter to un highlight search
nnoremap <CR> :nohlsearch<CR>


"""""""""""""
" Indenting "
"""""""""""""

set tabstop=4 shiftwidth=4 expandtab
set autoindent copyindent smartindent

"""""""""
" Theme "
"""""""""

syntax enable
colorscheme solarized
set background=dark

""""""""
" GVim "
""""""""

if has('gui_running')
    set gcr=n:blinkon0 guioptions-=m guifont=Monaco:h11 background=light
endif

"""""""""""""""""""""
" Language-Specific "
"""""""""""""""""""""

" load the plugin and indent settings for the detected filetype
filetype plugin indent on

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} set ft=ruby
au BufRead,BufNewFile *.html.erb set ft=eruby

" Add json syntax highlighting
au BufNewFile,BufRead *.json set ft=json syntax=javascript

" Add markdown syntax highlighting
au BufNewFile,BufRead *.md set ft=markdown 
