if exists('b:current_syntax')
    finish
endif

syntax region vlime_replComment start=/\m\([^\\]\@<=;\)\|\(^;\)/ end=/\m$/ contains=vlime_replWarning,vlime_replError,vlime_replConditionSummary keepend
syntax match vlime_replConditionSummary /\m\(\s\|^\)\@<=caught \d\+ .\+ conditions*\(\s\|$\)\@=/
syntax match vlime_replWarning /\m\(\s\|^\)\@<=\(\(WARNING\)\|\(STYLE-WARNING\)\):\(\s\|$\)\@=/
syntax match vlime_replError /\m\(\s\|^\)\@<=ERROR:\(\s\|$\)\@=/
syntax region vlime_replObject start=/\m#</ end=/\m>\($\|\_s\)\@=/
syntax region vlime_replString start=/\m"/ skip=/\m\\\\\|\\"/ end=/\m"/
syntax match vlime_replSeparator /\m^--$/

hi def link vlime_replSeparator Comment
hi def link vlime_replObject Constant
hi def link vlime_replString String
hi def link vlime_replComment Comment
hi def link vlime_replConditionSummary WarningMsg
hi def link vlime_replWarning WarningMsg
hi def link vlime_replError ErrorMsg

let b:current_syntax = 'vlime_repl'
