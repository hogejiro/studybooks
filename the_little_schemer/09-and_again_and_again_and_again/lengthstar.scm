(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (length '((a b) c)))
; 2
(print (length '((a b) (c d))))
; 2
(print (length '((a b) (c d e))))
; 2
