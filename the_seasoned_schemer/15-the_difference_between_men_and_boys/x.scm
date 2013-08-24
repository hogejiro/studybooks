(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(print "x: " x)
; x: (chicago pizza)
(set! x (quote gone))
(print "x: " x)
; x: gone
(define x (quote skins))
(print "x: " x)
; x: skins
