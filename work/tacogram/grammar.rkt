#lang brag

taco-program : new-line* taco-leaf new-line*
taco-leaf    : left-paren (not-a-taco | taco){7} right-paren
not-a-taco   : left-paren right-paren
taco         : /"%"

@left-paren  : /"#"
@right-paren : /"$"
@new-line    : /"\n"

; (parse-to-datum "\n##$%#$%#$#$#$$\n")
; =>
; '(taco-program
;   (taco-leaf
;    (not-a-taco)
;    (taco)
;    (not-a-taco)
;    (taco)
;    (not-a-taco)
;    (not-a-taco)
;    (not-a-taco)))