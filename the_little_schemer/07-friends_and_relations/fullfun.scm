(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
; #t
(print (fullfun? '((grape raisin) (plum prune) (stewed prune))))
; #f
(print (fullfun? '((grape raisin) (plum prune) (stewed grape))))
; #t
