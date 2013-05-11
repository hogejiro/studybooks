(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(print ((eq?-c 'salad) 'salad))
; #t
(define eq?-salad (eq?-c (quote salad)))
(print (eq?-salad 'salad))
; #t
(print (eq?-salad 'tuna))
; #f
