(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module names)
(print (let/cc Say (rm-a 'noodles '((food) more (food)) Say)))
; no
(print (let/cc Say (rm-b 'noodles '((food) more (food)) Say)))
; no
(print (let/cc Say (rm 'noodles '((food) more (food)) Say)))
; no
