(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (shuffle '(a (b c))))
; (a (b c))
(print (shuffle '(a b)))
; (a b)
; (print (shuffle '(a b) (c d)))
; undefine
