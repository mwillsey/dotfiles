" my vimrc

""""""""""
" Vundle "
""""""""""

set nocompatible

try
    filetype off                

    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/vundle
    call vundle#begin()

    " let Vundle manage Vundle, required
    Plugin 'gmarik/vundle'

    Plugin 'altercation/vim-colors-solarized'
    Plugin 'junegunn/vim-easy-align'
    Plugin 'tpope/vim-commentary'
    Plugin 'vim-pandoc/vim-pandoc'
    Plugin 'vim-pandoc/vim-pandoc-syntax'
    Plugin 'git://git.code.sf.net/p/vim-latex/vim-latex'
    Plugin 'dag/vim2hs'
    Plugin 'rking/ag.vim'
    Plugin 'wting/rust.vim'

    call vundle#end() 
    filetype plugin indent on 
catch
    echom "Something went wrong with Vundle, it probably doesn't exist. Run this: "
    echom "git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim"
    echom "vim +PluginInstall +qall"
endtry

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

" emacs-like spell correction with <c-l> that puts you back in insert
imap <C-l> <C-g>u<Esc>[s1z=`]A<C-g>u

"""""""""""""
" Dev Setup "
"""""""""""""

" open error window on make
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" look up recursively for a tags file
set tags=tags,./tags;/

"""""""""""
" Plugins "
"""""""""""

" easy-align in visual mode
vmap <CR> <Plug>(EasyAlign)

" pandoc
let g:pandoc#syntax#style#emphases=0
" no fold column
let g:pandoc#folding#fdc=0
" no pandoc conceal
let g:pandoc#syntax#conceal#use=0

"" latex-suite ""

let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 --interaction=nonstopmode $*'
let g:Tex_ViewRule_pdf = 'Skim'

" create align* mapping after startup
au VimEnter * call IMAP('EAL', "\\begin{align*}\<CR><++>\<CR>\\end{align*}<++>", 'tex')

" compile on write
au BufWritePost *.tex silent call Tex_RunLaTeX()

"" haskell ""

let g:haskell_autotags=1

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
set bg=light
set cursorline

""""""""
" GVim "
""""""""

if has('gui_running')
    set gcr=n:blinkon0 guioptions-=m guifont=Monaco:h11 
endif

"""""""""""""""""""""
" Language-Specific "
"""""""""""""""""""""

" spell check on not code
au FileType {markdown,text,tex} setlocal spell

" smaller indent in haskell
au FileType haskell setlocal shiftwidth=2 tabstop=2

" smaller indent in ocaml
au FileType ocaml setlocal shiftwidth=2 tabstop=2
autocmd FileType ocaml set commentstring=(\*\ %s\ \*)

" Add json syntax highlighting
au BufNewFile,BufRead *.json set ft=json syntax=javascript

" Add markdown syntax highlighting
au BufNewFile,BufRead *.md set ft=markdown 
