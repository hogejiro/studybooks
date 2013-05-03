(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
; #t
(print (fun? '((b 4) (b 0) (b 9) (e 5) (g 4))))
; #f
