(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (sub1 5))
; 4
(print (sub1 0))
; -1 
