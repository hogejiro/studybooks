(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shadows)
(print (value '(+ 3 4)))
; 7
(print (value '(+ (* 3 6) (% 8 2))))
; 82
