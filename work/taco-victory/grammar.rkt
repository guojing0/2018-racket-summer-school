#lang brag

taco-program : taco-leaf*
taco-leaf : (not-a-taco | taco){7}
taco : /"%"
not-a-taco : /"#$"