(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(set! x (quote onion))
(print (glutton (quote garlic)))
; (more garlic more garlic)
(print x)
; onion
