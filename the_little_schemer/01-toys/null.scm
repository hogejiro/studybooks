(print (null? ()))
; #t
(print (null? '()))
; #t
(print (null? (quote ())))
; #t
(print (null? '(a b c)))
; #f
(print (null? 'atom))
; #f
