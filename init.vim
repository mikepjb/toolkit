" init.vim - batteries included

func! EnsureManager()
  let plugPath = expand("~/.config/nvim/autoload/plug.vim")
  if !filereadable(plugPath)
    echo 'downloading plugin manager'
    echo system("curl -fLo " . plugPath . " --create-dirs "
          \. "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
  endif
endfunc
call EnsureManager()
call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'mikepjb/vim-pair'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()

set nu
set autowrite " write when using :make.
set hidden " allow backgrounding buffers without saving.
set shiftwidth=2
set tabstop=2
set gdefault " search/replace works across the entire line by default.
set clipboard=unnamedplus
set expandtab smarttab
set ignorecase " case insentive search.
set smartcase " case sensitive search when you use capitals.
set mouse=a " enable mouse
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.

let mapleader= ' '
nnoremap Y y$
nnoremap <C-q> :quit<cr>
nnoremap <leader>i :e ~/.config/nvim/init.vim<cr>
nnoremap <leader>f :FZF<cr>
nnoremap <leader>b :Buffers<cr>
imap <C-c> <esc>
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>

" command line behaves like readline
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" handy insert movements
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Delete>
inoremap <C-l> <space>=><space>

augroup go
  " remove all previous commands in this group
  au!
  let g:go_template_autocreate = 0 " do not pause ages creating a template.
  let g:go_fmt_command = "goimports" " include imports on save.
  au Syntax go nnoremap gD :GoDecls<cr>
augroup end

colorscheme lumo
