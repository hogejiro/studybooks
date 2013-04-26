(print (or (null? ()) (null? '(d e f g))))
; #t
(print (or (null? '(a b c)) (null? ())))
; #t
(print (or (null? '(a b c)) (null? '(atom))))
; #f
