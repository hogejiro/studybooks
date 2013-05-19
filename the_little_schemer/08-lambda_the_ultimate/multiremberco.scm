(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(define a-friend
 (lambda (x y)
  (null? y)))
(print (multirember&co 'tuna '(tuna) a-friend))
; #f
(define length
 (lambda (lat)
  (cond
   ((null? lat) 0)
   (else (+ 1 (length (cdr lat)))))))
(define last-friend
 (lambda (x y)
  (length x)))
(print (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
; 3
