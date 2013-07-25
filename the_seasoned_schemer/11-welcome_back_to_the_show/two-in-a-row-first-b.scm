(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shows)
(print (two-in-a-row-first-b? '(Italian sardines spaghetti parsley)))
; #f
(print (two-in-a-row-first-b? '(Italian sardines sardines spaghetti parsley)))
; #t
(print (two-in-a-row-first-b? '(Italian sardines more sardines spaghetti)))
; #f
