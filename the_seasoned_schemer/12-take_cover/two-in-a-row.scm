(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module covers)
(print (two-in-a-row?-a '(red blue blue green)))
; #t
(print (two-in-a-row?-b '(red blue blue green)))
; #t
