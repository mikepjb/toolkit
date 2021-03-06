" align parameters
" comment regions/visual selection
" markdown preview

syntax on " required for ruby indentation
filetype plugin indent on

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
set clipboard=unnamedplus
set ignorecase smartcase
set incsearch
set nu
set cul
set mouse=a
set list
let &listchars = "tab:\u21e5\u00b7,trail:\u2423"
set wildmenu
set nojoinspaces
set laststatus=1
set autoread
set gdefault
set hlsearch
set t_ti= t_te=
set isk+=-
" Normally, Vim messes with iskeyword when you open a shell file. This can
" leak out, polluting other file types even after a 'set ft=' change. This
" variable prevents the iskeyword change so it can't hurt anyone.
let g:sh_noisk=1
set completeopt -=preview
set omnifunc=Suggest
runtime macros/matchit.vim
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.

function! Suggest(findstart, base)
  if a:findstart == 1
    " gocode complete examines the current buffer itself..
    execute "silent let g:suggest_completions = " . system('suggest')
    return col('.') - g:suggest_completions[0] - 1
  else
    return g:suggest_completions[1]
  endif
endfunction

colorscheme lumo

let mapleader= ' '
map <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
inoremap <C-a> <C-o>0
inoremap <C-e> <C-o>$
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Delete>
inoremap <C-l> <space>=><space>
map <M-m> ^
map Q @q
map Y y$
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>
map <C-q> :quit<CR>
imap <C-c> <esc>
imap <C-t> <></><Esc>5h"gdiw"gp3l"gpT>i
vmap <C-t> :'<,'>!column -t<cr>
nnoremap gb :!git blame %<cr>

func! Pick(input_cmd, vim_cmd)
  try
    let selection = system(a:input_cmd . " | pick")
  catch /Vim:Interrupt/
  endtry
  redraw!
  if len(selection) != 0
    exec a:vim_cmd . " " . selection
  endif
endfunc

nnoremap <leader>f :call Pick("find * -type f", ":e")<cr>

function! SelectaBuffer()
  let bufnrs = filter(range(1, bufnr("$")), 'buflisted(v:val)')
  let buffers = map(bufnrs, 'bufname(v:val)')
  call Pick('echo "' . join(buffers, "\n") . '"', ":b")
endfunction

" Fuzzy select a buffer. Open the selected buffer with :b.
nnoremap <leader>b :call SelectaBuffer()<cr>

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

" align: '<,'>!sed $'s/::/\001::/' | column -ets $'\001'

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
  command! -nargs=+ -complete=file -bar Rg silent! grep! <args>|cwindow|redraw!
  nnoremap \ :Rg<SPACE>
  nnoremap <leader>g :Rg<SPACE>
endif

command! TrimWhitespace :%s/\s\+$//e
command! PrettifyJSON :%!python -m json.tool

function! MarkdownEnvironment()
  func! Foldexpr_markdown(lnum)
    let l1 = getline(a:lnum)

    if l1 =~ '^\s*$'
      " ignore empty lines
      return '='
    endif

    let l2 = getline(a:lnum+1)

    if  l2 =~ '^==\+\s*'
      " next line is underlined (level 1)
      return '>1'
    elseif l2 =~ '^--\+\s*'
      " next line is underlined (level 2)
      return '>2'
    elseif l1 =~ '^#'
      " current line starts with hashes
      return '>'.matchend(l1, '^#\+')
    elseif a:lnum == 1
      " fold any 'preamble'
      return '>1'
    else
      " keep previous foldlevel
      return '='
    endif
  endfunc

  setlocal foldexpr=Foldexpr_markdown(v:lnum)
  setlocal foldmethod=expr
endfunction

augroup markdown
  au! Filetype markdown :call MarkdownEnvironment()
augroup END

func! FoldOnComments()
  func! Foldexpr(lnum)
    let l1 = getline(a:lnum)
    let l2 = getline(a:lnum+1)

    if l1 =~ '^/.\?\*.*\\*.\?/$'
      return '>1'
    elseif l2 =~ '^/.\?\*.*\\*.\?/$'
      return '<1'
    else
      return '='
    endif
  endfunc

  setlocal foldexpr=Foldexpr(v:lnum)
  setlocal foldmethod=expr
endfunc

augroup commentFolding
  au! Filetype css :call FoldOnComments()
  au! Filetype javascript :call FoldOnComments()
augroup END

function! GolangEnvironment()
  setlocal tabstop=2
        \ shiftwidth=2
        \ softtabstop=2
        \ noexpandtab
        \ omnifunc=autoload#GoComplete
endfunction

function! GodefUnderCursor()
    let pos = getpos(".")[1:2]
    if &encoding == 'utf-8'
        let offs = line2byte(pos[0]) + pos[1] - 2
    else
        let c = pos[1]
        let buf = line('.') == 1 ? "" : (join(getline(1, pos[0] - 1), "\n") . "\n")
        let buf .= c == 1 ? "" : getline(pos[0])[:c-2]
        let offs = len(iconv(buf, &encoding, "utf-8"))
    endif
    " echo offs
    let source_location = 
          \split(system("godef -f=". expand("%:p") . " -o=" . offs), ":")

    exec "e +" . source_location[1] . " " . source_location[0]
endfunction

function! GuruUnderCursor(command)
    let pos = getpos(".")[1:2]
    if &encoding == 'utf-8'
        let offs = line2byte(pos[0]) + pos[1] - 2
    else
        let c = pos[1]
        let buf = line('.') == 1 ? "" : (join(getline(1, pos[0] - 1), "\n") . "\n")
        let buf .= c == 1 ? "" : getline(pos[0])[:c-2]
        let offs = len(iconv(buf, &encoding, "utf-8"))
    endif
    " echo offs
    let source_location = 
          \split(system("guru -scope '...' " . a:command . " ". expand("%:p") . ":#" . offs), ":")
    echo source_location

    " exec "e +" . source_location[1] . " " . source_location[0]
endfunction

function! GoFmt()
  let out = system("goimports -w " . expand("%"))
  if strlen(out) != 0
    echo out
  endif
  mkview
  execute ":silent e!"
  silent loadview
endfunction

augroup golang
  au! BufWritePost *.go :call GoFmt()
  " ! shouldn't be used on Syntax.. why? it works tho..
  au Syntax go syn match goAssign /:\?=/
  au Syntax go syn match goDeclaration /<func\>/ nextgroup=goFunction skipwhite skipnl
  au Syntax go syn match goFunction /\w\+\ze(/
  au! Filetype go :call GolangEnvironment()
  " au! Filetype go nnoremap gd :call GodefUnderCursor()<cr>
  nnoremap gd :call GodefUnderCursor()<cr>
  au! Filetype go let &makeprg = '('
        \.  'golint ' . expand('%') . ' ; '
        \. 'go test ./... -v ; '
        \. 'go vet ; '
        \. 'go build -o /tmp/' . system('echo -n $(uuidgen)')
        \. ')'
  au! BufNewFile,BufRead *.go setlocal nolist
augroup END

" automatically close/open quickfix when empty/holding info
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" Leave the return key alone when in command line windows, since it's used
" to run commands there.
augroup enter
  autocmd! CmdwinEnter * :unmap <cr>
  autocmd! CmdwinLeave * :call MapCR()
  autocmd! BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup END

function! MapCR()
  nnoremap <cr> :call RunTestFile()<cr>
endfunction
call MapCR()

function! RunTestFile(...)
  if a:0
    let command_suffix = a:1
  else
    let command_suffix = ""
  endif

  " Are we in a test file?
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|test_.*\.py\|_test.py\)$') != -1

  " Run the tests for the previously-marked file (or the current file if
  " it's a test).
  if &filetype == 'clojure'
    exec ":Load"
  elseif &filetype == 'scheme'
    exec ":!csi -s %"
  elseif &filetype == 'go' || &filetype == 'javascript'
    make
  else
    if in_test_file
      call SetTestFile(command_suffix)
    elseif !exists("t:grb_test_file")
      return
    end
    call RunTests(t:grb_test_file)
  endif
endfunction

function! SetTestFile(command_suffix)
  " Set the spec file that tests will be run for.
  let t:grb_test_file=@% . a:command_suffix
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    if expand("%") != ""
      :w
    end

    if match(a:filename, '\.feature$') != -1
      exec ":!script/features " . a:filename
    else
        " First choice: project-specific test script
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        " Fall back to the .test-commands pipe if available, assuming someone
        " is reading the other side and running the commands
        elseif filewritable(".test-commands")
          let cmd = 'rspec --color --format progress --require "~/lib/vim_rspec_formatter" --format VimFormatter --out tmp/quickfix'
          exec ":!echo " . cmd . " " . a:filename . " > .test-commands"

          " Write an empty string to block until the command completes
          sleep 100m " milliseconds
          :!echo > .test-commands
          redraw!
        " Fall back to a blocking test run with Bundler
        elseif filereadable("Gemfile")
            exec ":!bundle exec rspec --color " . a:filename
        " If we see python-looking tests, assume they should be run with Nose
        elseif strlen(glob("test/**/*.py") . glob("tests/**/*.py"))
            exec "!nosetests " . a:filename
        " Fall back to a normal blocking test run
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction

function! OpenPair(left, right)
  return a:left . a:right . "\<Left>"
endfunction

function! ClosePair(left, right)
    let current_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if current_char == a:right
        return "\<Right>"
    else
        return a:right
    endif
endfunction

function! DeletePair()
    let previous_char = matchstr(getline('.'), '\%' . (col('.')-1) . 'c.')
    let current_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if (current_char == ")" && previous_char == "(") ||
                \ (current_char == "]" && previous_char == "[") ||
                \ (current_char == "}" && previous_char == "{") ||
                \ (current_char == "\"" && previous_char == "\"")
        return "\<Left>\<C-o>2s"
    elseif previous_char == ")"
        return "\<Left>"
    else
        return "\<BS>"
    endif
endfunction

inoremap <expr> ( OpenPair("(",")")
inoremap <expr> [ OpenPair("[","]")
inoremap <expr> { OpenPair("{","}")
inoremap <expr> ) ClosePair("(",")")
inoremap <expr> ] ClosePair("[","]")
inoremap <expr> } ClosePair("{","}")
inoremap <expr>  DeletePair()

function! SyntaxStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

command! SyntaxStack :call SyntaxStack()

function! Delete()
  !rm %
  bunload
endfunc

command! Delete :call Delete()

let r_indent_align_args = 0
let r_indent_ess_compatible = 0

augroup lisp
  au!
  autocmd BufNewFile,BufReadPost *.boot set filetype=clojure
  autocmd Syntax clojure match clojureConstant "defn \zs\(\w\|-\)*"
augroup END

let g:clojure_align_subforms = 0
let g:clojure_fuzzy_indent = 1
let g:clojure_align_multiline_strings = 1
let g:clojure_fuzzy_indent_patterns = ['^ns', '^def', '^fn', 'let$']

function! Govern()
  exec ":!govern -file " . expand("%")
endfunction

func! Require()
  " echo('clji "(load-file \"' . expand('%') . '\")"')
  let call = system('clji "(load-file \"' . expand('%') . '\")"')
  " let call = system("clji \"(require 'blue.strategic.billing.backing-sheets :reload)\"")
  echo call
endfunc

command! Require :call Require()

command! -nargs=? -range Eval <line1>,<line2>call Eval('<args>')
vnoremap <CR> :Eval<CR>
function! Eval(regex) range
  let section = getline(a:firstline, a:lastline)
  let call = system("clji \"" . join(section) . "\"")
  echo call
endfunction
