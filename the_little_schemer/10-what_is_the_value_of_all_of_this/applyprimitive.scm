(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module values)
(print (apply-primitive 'cons '(6 (a b c))))
; (6 a b c)
