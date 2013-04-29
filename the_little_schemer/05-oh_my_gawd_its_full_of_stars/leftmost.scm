(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module stars)
(print (leftmost '((potato) (chips ((with) fish) (chips)))))
; potato
(print (leftmost '(((hot) (tuna (and))) cheese)))
; hot
; (print (leftmost '(((() four)) 17 (seventeen))))
; gosh: "error": pair required, but got ()
; (print (leftmost (quote ())))
; gosh: "error": pair required, but got ()
