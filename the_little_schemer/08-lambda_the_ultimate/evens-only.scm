(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(print (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))
