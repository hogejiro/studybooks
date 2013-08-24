(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module differences)
(adder-no-closure)
; 1
(adder-no-closure)
; 1
(adder-no-closure)
; 1
(adder-closure)
; 1
(adder-closure)
; 2
(adder-closure)
; 3
