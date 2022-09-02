if exists('b:current_syntax')
    finish
endif

syntax clear

syntax match vlime_sldbSection /\m^Thread: \d\+; Level: \d\+$/ contains=vlime_sldbNumber
syntax match vlime_sldbSection /\m^Restarts:$/
syntax match vlime_sldbSection /\m^Frames:$/
syntax match vlime_sldbRestart /\m\(^\s*\d\+\.\s\+\)\@<=[^[:space:]]\+\(\s\+-\)\@=/
syntax match vlime_sldbNumber "-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syntax match vlime_sldbNumber "-\=\(\d\+/\d\+\)"
syntax region vlime_sldbString start=/\m"/ skip=/\m\\\\\|\\"/ end=/\m/ end=/$/me=e-1
syntax region vlime_sldbObject start=/\m#</ end=/\m>/ end=/$/me=e-1 contains=vlime_sldbObject,vlime_sldbString


syntax match vlime_sldbHERE /\V***HERE***/ contained
syntax region vlime_sldbSnippet start=/^\tSnippet:/ end=/^\tLocals:/me=e-8 contains=vlime_sldbParen,vlime_sldbHEREParen keepend
syntax region vlime_sldbParen start=/`\=(/ skip=/|.\{-}|/ end=/)/ contained contains=vlime_sldbParen,vlime_sldbHEREParen

" vlime_sldbParen,
" TODO: initial ( is already Paren "only", want a different color!
syntax region vlime_sldbHEREParen start=/\V***HERE*** (/ skip=/|.\{-}|/ end=/)/ contained contains=vlime_sldbHERE


hi def link vlime_sldbSection Comment
hi def link vlime_sldbRestart Operator
hi def link vlime_sldbNumber Number
hi def link vlime_sldbString String
hi def link vlime_sldbParen Identifier
hi def link vlime_sldbObject Constant
hi def link vlime_sldbHERE Error
hi def link vlime_sldbHEREParen Warning

let b:current_syntax = 'vlime_sldb'
