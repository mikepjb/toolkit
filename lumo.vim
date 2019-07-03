" Lumo
" Author:       Michael Bruce <http://michaelbruce.co/>
" vim:set ts=2 sts=2 sw=2 expandtab:

hi clear
set background=dark

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "lumo"

let s:col = {
      \ 'none' : 'NONE',
      \ 'italic' : 'italic',
      \ 'underline' : 'underline',
      \ 'black': 0,
      \ 'black-light': 8,
      \ 'red': 1,
      \ 'red-light': 9,
      \ 'green': 2,
      \ 'green-light': 10,
      \ 'yellow': 3,
      \ 'yellow-light': 11,
      \ 'blue': 4,
      \ 'blue-light': 12,
      \ 'magenta': 5,
      \ 'magenta-light': 13,
      \ 'cyan': 6,
      \ 'cyan-light': 14,
      \ 'white': 7,
      \ 'white-light': 15,
      \ }

let s:highlights = [
      \['Normal', 'none', 'none', 'none', 'none'],
      \['Number', 'magenta', 'none', 'none', 'none'],
      \['Function', 'green', 'none', 'none', 'none'],
      \['Identifier', 'none', 'none', 'none', 'none'],
      \['Preproc', 'red', 'none', 'none', 'none'],
      \['Statement', 'red', 'none', 'none', 'none'],
      \['Comment', 'white', 'none', 'italic', 'none'],
      \['Type', 'blue', 'none', 'italic', 'none'],
      \['TODO', 'yellow', 'none', 'italic', 'none'],
      \['String', 'yellow-light', 'none', 'none', 'none'],
      \['Quote', 'yellow', 'none', 'none', 'none'],
      \['CursorLine', 'none', 'black', 'none', 'none'],
      \['Search', 'none', 'none', 'underline', 'none'],
      \['Visual', 'none', 'black', 'none', 'none'],
      \['CursorLineNr', 'yellow-light', 'black', 'none', 'none'],
      \['LineNr', 'white', 'none', 'none', 'none'],
      \['Error', 'red', 'none', 'none', 'none'],
      \['ErrorMsg', 'red', 'none', 'none', 'none'],
      \['WarningMsg', 'yellow-light', 'none', 'none', 'none'],
      \['Question', 'yellow-light', 'none', 'none', 'none'],
      \['NonText', 'white', 'none', 'none', 'none'],
      \['Block', 'green', 'none', 'none', 'none'],
      \['Operator', 'red', 'none', 'none', 'none'],
      \['MatchParen', 'green', 'none', 'none', 'none'],
      \['Folded', 'none', 'black', 'none', 'none'],
      \['FoldColumn', 'none', 'black', 'none', 'none'],
      \['diffAdded', 'green', 'none', 'none', 'none'],
      \['diffChange', 'yellow-light', 'none', 'none', 'none'],
      \['diffRemoved', 'red', 'none', 'none', 'none'],
      \['diffDelete', 'red', 'none', 'none', 'none'],
      \['DiffAdd', 'green', 'none', 'none', 'none'],
      \['DiffChange', 'yellow-light', 'none', 'none', 'none'],
      \['DiffText', 'yellow-light', 'none', 'none', 'none'],
      \['DiffDelete', 'red', 'none', 'none', 'none'],
      \['gitcommitBranch', 'red', 'none', 'none', 'none'],
      \['qfFileName', 'red', 'none', 'none', 'none'],
      \['PMenu', 'none', 'black', 'none', 'none'],
      \['PMenuSel', 'red', 'none', 'none', 'none'],
      \['User1', 'yellow-light', 'none', 'none', 'none'],
      \['StatusLine', 'white-light', 'none', 'none', 'none'],
      \['StatusLineNC', 'white', 'none', 'none', 'none'],
      \['VertSplit', 'white', 'none', 'none', 'none']
      \]
" TODO rubySymbol should be done at a general level - like var name..?

for hl in s:highlights
  exec 'hi! ' . hl[0]
        \. ' ctermfg=' . s:col[hl[1]]
        \. ' ctermbg=' . s:col[hl[2]]
        \. ' term=' . s:col[hl[3]]
        \. ' cterm=' . s:col[hl[4]]
endfor

" goParen/goBlock -> NonText does not appear to make a difference.
let s:links = [
      \['vimString', 'String'],
      \['Delimiter', 'NonText'],
      \['goConstants', 'Number'],
      \['goParen', 'Delimiter'],
      \['goBlock', 'Delimiter'],
      \['rubyBlock', 'Delimiter'],
      \['rubyStringDelimiter', 'Delimiter'],
      \['rubyString', 'String'],
      \['rubySymbol', 'Function'],
      \['goEscapeC', 'Operator'],
      \['goEscapeOctal', 'Operator'],
      \['goAssign', 'Operator'],
      \['goDeclaration', 'Statement'],
      \['goFunction', 'Function'],
      \['goBuiltins', 'Function'],
      \['yamlBlockMappingKey', 'Function']
      \]

for l in s:links
  exec 'hi! def link ' . l[0] . ' ' . l[1]
endfor

set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
set fillchars=stlnc:\-,stl:\-,vert:\|
