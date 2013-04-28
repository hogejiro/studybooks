(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (power 1 1))
; 1
(print (power 2 3))
; 8
(print (power 5 3))
; 125
