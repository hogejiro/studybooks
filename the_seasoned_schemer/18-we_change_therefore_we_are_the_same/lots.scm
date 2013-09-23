(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module changesames)
(print (lots 3))
; #<closure (bons bons)>
(print (lenkth (lots 3)))
; 3
(print (lenkth (kons (quote egg) (lots 3))))
; 4
