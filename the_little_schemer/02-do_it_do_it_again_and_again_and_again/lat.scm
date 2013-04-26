(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module misc)
(print (lat? '(Jack (Spart could) eat no chiecken fat)))
; #f
(print (lat? '(bacon and eggs)))
; #t
(print (lat? '(bacon (and eggs))))
; #f
