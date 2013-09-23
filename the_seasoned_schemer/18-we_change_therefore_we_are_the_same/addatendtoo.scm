(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module changesames)
(set-kounter 0)
(print (add-at-end-too (lots 3)))
; #<closure (bons bons)>
(print (kounter))
; 0
