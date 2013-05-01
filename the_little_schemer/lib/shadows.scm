(define-module shadows
 (define %
  (lambda (n m)
   (cond
    ((= m 0) 1)
    (else (* n (% n (- m 1)))))))
 (define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))
 (define numbered?
  (lambda (aexp)
   (cond
    ((atom? aexp) (number? aexp))
    (else
     (and (numbered? (car aexp))
      (numbered? (car (cdr (cdr aexp)))))))))
 (define first-sub-exp
  (lambda (aexp)
   (car (cdr aexp))))
 (define second-sub-exp
  (lambda (aexp)
   (car (cdr (cdr aexp)))))
 (define operator
  (lambda (aexp)
   (car aexp)))
 (define value
  (lambda (nexp)
   (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) (quote +))
     (+ (value (first-sub-exp nexp))
      (value (second-sub-exp nexp))))
    ((eq? (operator nexp) (quote *))
     (* (value (first-sub-exp nexp))
      (value (second-sub-exp nexp))))
    (else
     (% (value (first-sub-exp nexp))
      (value (second-sub-exp nexp)))))))
 (define sero?
  (lambda (n)
   (null? n)))
 (define edd1
  (lambda (n)
   (cons (quote()) n)))
 (define zub1
  (lambda (n)
   (cdr n)))
 (define plus
  (lambda (n m)
   (cond
    ((sero? m) n)
    (else (edd1 (plus n (zub1 m)))))))
 (define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))
 (define lat?
  (lambda (l)
   (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f))))
 (export shadows)
)
