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

    Plugin 'junegunn/vim-easy-align'
    Plugin 'tpope/vim-commentary'

    call vundle#end() 
    filetype plugin indent on 
catch
    echom "Something went wrong with Vundle, it probably doesn't exist."
    if confirm("Should we try to fix it?", "n\ny") == 2
        !git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/vundle
        echom "Vundle installed, run PluginInstall in vim to update plugins"
        qall
    else
        echom "ok we won't fix it"
    endif
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

" highlight lines over 80 characters
highlight LongLines ctermbg=red ctermfg=white guibg=#592929
match LongLines /\%81v.\+/

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


"""""""""""""
" Searching "
"""""""""""""

set hlsearch incsearch ignorecase smartcase gdefault 

" enter to un highlight search
nnoremap <CR> :nohlsearch<CR>


"""""""""""""
" Indenting "
"""""""""""""
filetype plugin indent on
set tabstop=4 shiftwidth=4 expandtab autoindent 

"""""""""
" Theme "
"""""""""

syntax enable
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
