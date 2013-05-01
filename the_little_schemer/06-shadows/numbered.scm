(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shadows)
(print (numbered? '1))
; #t
(print (numbered? '(3 + (4 * 5))))
; #t
(print (numbered? '(3 + (4 ^ 5))))
; #t
(print (numbered? '(2 * sausage)))
; #f
