(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(define length
 (lambda (lat)
  (cond
   ((null? lat) 0)
   (else (+ 1 (length (cdr lat)))))))
(define lengthandinserts
 (lambda (lat x y)
  (+ (length lat) (+ x y))))
(print (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) lengthandinserts))
; 15
