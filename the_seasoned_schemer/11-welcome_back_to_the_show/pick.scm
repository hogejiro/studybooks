(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module shows)
(print (pick 4 '(4 3 1 1 1)))
; 1
(print (pick 2 '(2 4 3 1 1 1)))
; 4
