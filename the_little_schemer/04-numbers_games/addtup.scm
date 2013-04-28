(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (addtup '(3 5 2 8)))
; 18
