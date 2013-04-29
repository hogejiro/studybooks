(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module toys)
(print (atom? 'Harry))
; #t
(print (atom? '()))
; #f
(print (atom? '(car (cdr '(swing low sweet cherry oat)))))
; #f
(print (atom? (car (cdr '(swing low sweet cherry oat)))))
; #t
