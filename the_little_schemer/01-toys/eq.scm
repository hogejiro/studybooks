(print (eq? () ()))
; #t
(print (eq? 7 6))
; #f
(print (eq? 7 7))
; #t
(print (eq? (car ' (beans beans we need jelly beans)) 
        (car (cdr ' (beans beans we need jelly beans)))))
; #t
