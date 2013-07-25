(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shows)
(print (two-in-a-row-first-a? '(Italian sardines spaghetti parsley)))
; #f
(print (two-in-a-row-first-a? '(Italian sardines sardines spaghetti parsley)))
; #t
(print (two-in-a-row-first-a? '(Italian sardines more sardines spaghetti)))
; #f
