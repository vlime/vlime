if exists('b:current_syntax')
    finish
endif

syntax region vlime_inspectorObject start=/\m#</ end=/\m>/ contains=vlime_inspectorObject
syntax region vlime_inspectorString start=/\m"/ skip=/\m\\\\\|\\"/ end=/\m"/
syntax match vlime_inspectorNumber "-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syntax match vlime_inspectorNumber "-\=\(\d\+/\d\+\)"
syntax region vlime_inspectorButton start=/\m\[/ end=/\m\]/

hi def link vlime_inspectorObject Constant
hi def link vlime_inspectorString String
hi def link vlime_inspectorNumber Number
hi def link vlime_inspectorButton Operator

let b:current_syntax = 'vlime_inspector'
