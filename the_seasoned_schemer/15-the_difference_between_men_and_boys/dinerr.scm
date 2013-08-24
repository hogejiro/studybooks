(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(print (dinerR (quote onion)))
; (milkshake onion)
(print x)
; onion
(print (dinerR (quote pecanpie)))
; (milkshake pecanpie)
(print x)
; pecanpie
(print (gourmand (quote onion)))
; (onion onion)
(print x)
; onion
