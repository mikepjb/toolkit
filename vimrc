syntax on

set encoding=utf-8
set hidden
set expandtab smarttab
set showmatch
set shiftwidth=2
set tabstop=2
set backspace=2
set autoindent
set textwidth=80
set shortmess=aIT
set noswapfile nobackup nowritebackup
set clipboard=unnamed,unnamedplus
set ignorecase smartcase
set incsearch
set list
let &listchars = "tab:\u21e5\u00b7,trail:\u2423"
set wildmenu
set nojoinspaces
set autoread
set gdefault
filetype indent on

colorscheme night

let mapleader= ' '

let r_indent_align_args = 0
let r_indent_ess_compatible = 0

cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>

inoremap <C-f> <Right>
inoremap <C-b> <Left>

map Q @q
map Y y$
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>
map <C-q> :quit<CR>

function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction

nnoremap <leader>f :call SelectaCommand("find * -type f", "", ":e")<cr>

command! -nargs=? -range Align <line1>,<line2>call AlignSection('<args>')
vnoremap <silent> <Leader>a :Align<CR>
function! AlignSection(regex) range
  let extra = 1
  let sep = empty(a:regex) ? '=' : a:regex
  let maxpos = 0
  let section = getline(a:firstline, a:lastline)
  for line in section
    let pos = match(line, ' *'.sep)
    if maxpos < pos
      let maxpos = pos
    endif
  endfor
  call map(section, 'AlignLine(v:val, sep, maxpos, extra)')
  call setline(a:firstline, section)
endfunction

function! AlignLine(line, sep, maxpos, extra)
  let m = matchlist(a:line, '\(.\{-}\) \{-}\('.a:sep.'.*\)')
  if empty(m)
    return a:line
  endif
  let spaces = repeat(' ', a:maxpos - strlen(m[1]) + a:extra)
  return m[1] . spaces . m[2]
endfunction

command! Load call LoadClojureFile()
function! LoadClojureFile()
  update
  !hurl "(load-file \"%:p\")"
endfunction

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
  command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
  nnoremap \ :Ag<SPACE>
endif

command! TrimWhitespace :%s/\s\+$//e

command! Format execute ":silent !gofmt -w %" | execute "redraw!"
