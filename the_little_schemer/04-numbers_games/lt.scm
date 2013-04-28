(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (lt 4 6))
; #t
(print (lt 8 3))
; #f
(print (lt 6 6))
; #f
