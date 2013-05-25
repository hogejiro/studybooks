(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (A '1 '0))
; 2
(print (A '1 '1))
; 3
(print (A '2 '2))
; 7
