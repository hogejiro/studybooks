(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(print (atom-to-function (operator '(+ 5 3))))
; #<subr +>
(print (value (% (* 5 2) 3)))
; 1000
