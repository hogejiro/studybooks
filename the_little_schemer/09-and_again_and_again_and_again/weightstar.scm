(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (weight* '((a b) c)))
; 7
(print (weight* '(a (b c))))
; 5
