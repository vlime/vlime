if exists('b:current_syntax')
    finish
endif

syntax region vlime_traceObject start=/\m#</ end=/\m>/ contains=vlime_traceObject
syntax region vlime_traceString start=/\m"/ skip=/\m\\\\\|\\"/ end=/\m"/
syntax match vlime_traceNumber "-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syntax match vlime_traceNumber "-\=\(\d\+/\d\+\)"
syntax region vlime_traceButton start=/\m\[/ end=/\m\]/

hi def link vlime_traceObject Constant
hi def link vlime_traceString String
hi def link vlime_traceNumber Number
hi def link vlime_traceButton Operator

let b:current_syntax = 'vlime_trace'
