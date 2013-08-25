(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(print (deep-a 3))
; (((pizza)))
(print (deep-a 7))
; (((((((pizza)))))))
(print (deep-a 0))
; pizza
