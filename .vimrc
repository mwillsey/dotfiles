" my vimrc

""""""""""
" Vundle "
""""""""""

set nocompatible
set shell=bash

function SetupVundle()
    filetype off                

    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/vundle
    call vundle#begin()

    " let Vundle manage Vundle, required
    Plugin 'gmarik/vundle'

    Plugin 'junegunn/vim-easy-align'
    Plugin 'tpope/vim-commentary'
    Plugin 'git://git.code.sf.net/p/vim-latex/vim-latex'
    Plugin 'rking/ag.vim'
    Plugin 'kana/vim-textobj-user'
    Plugin 'roman/golden-ratio'

    call vundle#end() 
    filetype plugin indent on 
endfunction

try
    call SetupVundle()
catch
    echom "Something went wrong with Vundle, it probably doesn't exist."
    if confirm("Should we try to fix it?", "n\ny") == 2
        !mkdir -p ~/.vim/bundle
        !git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/vundle
        call SetupVundle()
        PluginInstall
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

" scrolling speed = 2 in terminal
if has("gui_running")
else
    nnoremap <ScrollWheelUp>   <C-Y> <C-Y>
    nnoremap <ScrollWheelDown> <C-E> <C-E>
    inoremap <ScrollWheelUp>   <C-Y> <C-Y>
    inoremap <ScrollWheelDown> <C-E> <C-E>
    vnoremap <ScrollWheelUp>   <C-Y> <C-Y>
    vnoremap <ScrollWheelDown> <C-E> <C-E>
endif


" highlight lines over 80 characters and dont type them in the first place
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/
set textwidth=80

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

""" latex-suite

let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 --interaction=nonstopmode $*'
let g:Tex_ViewRule_pdf = 'Skim'

" create align* mapping after startup
au VimEnter * call IMAP('EAL', "\\begin{align*}\<CR><++>\<CR>\\end{align*}<++>", 'tex')

" compile on write
au BufWritePost *.tex silent call Tex_RunLaTeX()

""" textobj-user
call textobj#user#plugin('latex', {
\   'environment': {
\     'pattern': ['\\begin{[^}]*}', '\\end{[^}]*}'],
\     'select-a': 'ae',
\     'select-i': 'ie',
\   },
\  'dollar-math-a': {
\     '*pattern*': '[$][^$]*[$]',
\     'select': 'a$',
\   },
\  'dollar-math-i': {
\     '*pattern*': '[$]\zs[^$]*\ze[$]',
\     'select': 'i$',
\   },
\ })

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

