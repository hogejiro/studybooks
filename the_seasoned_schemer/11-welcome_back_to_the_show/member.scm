(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shows)
(print (member 'sardines '(Italian sardines spaghetti parsley)))
; #t
