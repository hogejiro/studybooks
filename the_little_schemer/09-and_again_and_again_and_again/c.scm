(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (C '5))
; 1
(print (C '10))
; 1
