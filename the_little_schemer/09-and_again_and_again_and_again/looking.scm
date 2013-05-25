(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (looking 'caviar '(6 2 4 caviar 5 7 3)))
; #t
(print (looking 'caviar '(6 2 grits caviar 5 7 3)))
; #f
; (print (looking 'caviar '(7 1 2 caviar 5 6 3)))
; undefine
