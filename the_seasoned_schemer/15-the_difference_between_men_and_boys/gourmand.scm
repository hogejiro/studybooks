(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(set! x (quote rings))
(print "x: " x)
; x: rings
(print (gourmand (quote potato)))
; (potato potato)
(print "x: " x)
; x: potato
(print (gourmand 'rice))
; (rice rice)
(print "x: " x)
; x: rice
