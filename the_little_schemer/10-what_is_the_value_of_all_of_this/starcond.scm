(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module values)
(print (*cond '(cond (coffee klatsch) (t party)) '(((coffee) (t)) ((klatsch party) (5 (6))))))
; 5
(print (meaning 'coffee '(((coffee) (t)) ((klatsch party) (5 (6))))))
; (6)
