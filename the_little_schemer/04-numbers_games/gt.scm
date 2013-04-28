(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (gt 12 133))
; #f
(print (gt 120 11))
; #t
