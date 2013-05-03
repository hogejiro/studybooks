(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (one-to-one? '((chocolate chip) (doughy cookie))))
; #t
