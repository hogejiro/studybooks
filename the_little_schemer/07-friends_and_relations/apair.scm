(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (a-pair? '(pear pear)))
; #t
(print (a-pair? '(3 7)))
; #t
(print (a-pair? '((2) (pair))))
; #t
(print (a-pair? '(full (house))))
; #t
