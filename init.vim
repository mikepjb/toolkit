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

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'


set nu
set shiftwidth=2
set tabstop=2
set clipboard=unnamedplus
set expandtab smarttab
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.

let mapleader= ' '
nnoremap <leader>i :e ~/.config/nvim/init.vim<cr>
imap <C-c> <esc>
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>

colorscheme lumo
