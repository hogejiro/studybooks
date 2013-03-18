(define atom?
 (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(print (atom? ' Harry))
(print (atom? ' ()))
(print (atom? ' (car (cdr ' (swing low sweet cherry oat)))))
(print (atom? (car (cdr ' (swing low sweet cherry oat)))))
