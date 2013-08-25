(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module changes)
(print (supercounter deep-b))
; 500500
(set-counter 0)
(print (deepM-b 7))
; (((((((pizza)))))))
(print (supercounter deepM-b))
; 1000 
