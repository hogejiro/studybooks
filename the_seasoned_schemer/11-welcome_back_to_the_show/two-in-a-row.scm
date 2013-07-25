(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shows)
(print (two-in-a-row? '(b d e i i a g)))
; #t
