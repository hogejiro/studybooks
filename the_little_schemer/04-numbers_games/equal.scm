(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (equal 1 2))
; #f
(print (equal 2 2))
; #t
