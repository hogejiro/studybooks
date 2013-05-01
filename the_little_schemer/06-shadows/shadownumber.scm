(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shadows)
(print (+ 1 2))
; 3
(print (plus '(()) '(() ())))
; (() () ())
(print (lat? '(1 2 3)))
; #t
(print (lat? '((()) (() ()) (() () ()))))
; #f
