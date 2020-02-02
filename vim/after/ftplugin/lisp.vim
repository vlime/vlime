

" So that indentexpr is actually used
setlocal nolisp

" 20:06 < MarcoHinzGitter[> flip214:
syntax match SpellIgnore /\<\u\+\>/ contains=@NoSpell
 
hi SpellBad ctermbg=30
hi SpellCap ctermbg=90
