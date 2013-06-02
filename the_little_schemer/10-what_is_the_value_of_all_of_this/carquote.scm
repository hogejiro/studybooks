(print (car (quote (a b c))))
; a
(print (cons 'a
        (cons 'b
         (cons 'c
          (quote ())))))
; (a b c)
(print (cons 'car
        (cons (cons 'quote
               (cons
                (cons 'a
                 (cons 'b
                  (cons 'c
                   (quote ()))))
                (quote ())))
         (quote ()))))
; (car '(a b c))

