(define-module toys
 (define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))
 (export toys)
)
