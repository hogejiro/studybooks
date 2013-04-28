(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (length '(hotdogs with mustard sauerkraut and pickles)))
; 6
(print (length '(ham and cheese on rye)))
; 5
