(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module conses)
(print (insertR 'topping 'fudge '(ice cream with fudge for dessert)))
; (ice cream with fudge topping for dessert)
(print (insertR 'e 'd '(a b c d f g d h)))
; (a b c d e f g d h)
