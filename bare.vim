" Bare
" Author:       Michael Bruce <http://michaelbruce.online/>
" vim:set ts=2 sts=2 sw=2 expandtab:

set background=dark
hi clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "bare"

let s:col = {
      \ 'fg': 0,
      \ 'bg': 7,
      \ 'dark': 248,
      \ 'darker': 243,
      \ 'darkest': 239,
      \ 'light': 253,
      \ 'error': 1,
      \ 'green': 10,
      \ 'grey': 8,
      \ 'diffred': 5,
      \ 'red': 1
      \ }

function! s:setHighlight(name, fg, bg, display)
  exec 'hi ' . a:name . ' ' .
        \ 'ctermbg=' . a:bg . ' ' .
        \ 'ctermfg=' . a:fg . ' ' .
        \ 'cterm=' . a:display
endfunction

call s:setHighlight('Normal', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Number', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Function', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Statement', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('String', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Identifier', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Preproc', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Type', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('Comment', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('TODO', s:col['error'], s:col['bg'], 'NONE')
call s:setHighlight('StatusLine', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('StatusLineNC', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('VertSplit', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('Search', s:col['dark'], s:col['bg'], 'underline')
call s:setHighlight('LineNr', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('CursorLine', 'NONE', s:col['darkest'], 'NONE')
call s:setHighlight('Visual', s:col['bg'], s:col['fg'], 'NONE')
call s:setHighlight('Error', s:col['error'], s:col['bg'], 'NONE')
call s:setHighlight('ErrorMsg', s:col['error'], s:col['bg'], 'NONE')
call s:setHighlight('WarningMsg', s:col['error'], s:col['bg'], 'NONE')
call s:setHighlight('Question', s:col['error'], s:col['bg'], 'NONE')
call s:setHighlight('NonText', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('Delimiter', s:col['dark'], s:col['bg'], 'NONE')
call s:setHighlight('MatchParen', s:col['fg'], s:col['dark'], 'NONE')
call s:setHighlight('Operator', s:col['fg'], s:col['bg'], 'NONE')
" For git?!?
call s:setHighlight('diffAdded', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('diffChange', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('diffRemoved', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('diffDelete', s:col['fg'], s:col['bg'], 'NONE')
" For :diffthis & vim -d file1 file2
call s:setHighlight('DiffAdd', s:col['fg'], s:col['green'], 'NONE')
call s:setHighlight('DiffChange', s:col['fg'], s:col['grey'], 'NONE')
call s:setHighlight('DiffText', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('DiffDelete', s:col['fg'], s:col['diffred'], 'NONE')
call s:setHighlight('PMenu', s:col['dark'], s:col['darkest'], 'NONE')
call s:setHighlight('PMenuSel', s:col['fg'], s:col['darkest'], 'NONE')
call s:setHighlight('Folded', s:col['fg'], s:col['light'], 'NONE')
call s:setHighlight('FoldColumn', s:col['fg'], s:col['light'], 'NONE')

call s:setHighlight('rubyBlock', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('rubySymbol', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('rubyInterpolationDelimiter', s:col['fg'], s:col['bg'], 'NONE')
call s:setHighlight('rubyStringDelimiter', s:col['fg'], s:col['bg'], 'NONE')

call s:setHighlight('clojureSpecial', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('clojureDispatch', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('clojureSexp', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('clojureAnonArg', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('clojureMeta', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('clojureVarArg', s:col['red'], s:col['bg'], 'NONE')

call s:setHighlight('goEscapeC', s:col['red'], s:col['bg'], 'NONE')

call s:setHighlight('markdownH1', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('markdownH2', s:col['red'], s:col['bg'], 'NONE')

call s:setHighlight('vimMapModKey', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('vimNotation', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('helpSpecial', s:col['red'], s:col['bg'], 'NONE')
call s:setHighlight('helpNotVi', s:col['red'], s:col['bg'], 'NONE')

call s:setHighlight('shOption', s:col['red'], s:col['bg'], 'NONE')

call s:setHighlight('User1', s:col['dark'], s:col['bg'], 'NONE')
set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
set fillchars=stlnc:\-,stl:\-,vert:\|

