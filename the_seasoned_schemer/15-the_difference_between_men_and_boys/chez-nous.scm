(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(set! x (quote onion))
(print "x   : " x)
; x   : onion
(print "food: "food)
; food: none
(print (glutton (quote garlic)))
; (more garlic more garlic)
(print "x   : " x)
; x   : onion
(print "food: "food)
; food: garlic
(gourmand (quote potato))
(print "x   : " x)
; x   : potato
(print "food: "food)
; food: garlic
(chez-nous)
(print "x   : " x)
; x   : garlic
(print "food: "food)
; food: potato
