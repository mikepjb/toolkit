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
Plug 'tpope/vim-commentary'
Plug 'mikepjb/vim-pair'
Plug 'mikepjb/vim-fold', { 'for': ['css', 'markdown', 'javascript']}

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

Plug 'tpope/vim-fireplace', { 'for': ['clojure']}

Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim' 
Plug 'maxmellon/vim-jsx-pretty'

Plug 'junegunn/vim-easy-align'
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

setglobal tags=./tags;

let mapleader= ' '
nnoremap Y y$
nnoremap Q @q
nnoremap gb :Gblame<cr>
nnoremap <C-q> :quit<cr>
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :FZF<cr>
nnoremap <leader>g :FZF ~/notes<cr>
nnoremap <leader>i :e ~/.config/nvim/init.vim<cr>
imap <C-c> <esc>
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>

" easy-align supports regex with <C-x> from interactive mode
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

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

command! PrettifyJSON :%!python -m json.tool

augroup go
  " remove all previous commands in this group
  au!
  let g:go_template_autocreate = 0 " do not pause ages creating a template.
  au Syntax go nnoremap gD :GoDecls<cr>
  au Syntax go nnoremap gI :GoImports<cr>
  au Syntax go nnoremap gt :GoTest<cr>
augroup end

" augroup typescript
"   au!
"   au BufNewFile,BufRead *.ts setf typescript
"   au BufNewFile,BufRead *.tsx setf typescript
" augroup end

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  nnoremap L :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
  command! -nargs=+ -complete=file -bar Rg silent! grep! <args>|cwindow|redraw!
  nnoremap \ :Rg<SPACE>
endif

colorscheme lumo

augroup clojure
  au Syntax clojure nmap <buffer>  gd <Plug>FireplaceDjump
augroup end
