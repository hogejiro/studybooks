(define-module nums
 (define add1
  (lambda (n)
   (+ n 1)))
 (define sub1
  (lambda (n)
   (- n 1)))
 (define plus
  (lambda (n m)
   (cond
    ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))
 (define minus
  (lambda (n m)
   (cond
    ((zero? m) n)
    (else (sub1 (minus n (sub1 m)))))))
 (define addtup
  (lambda (tup)
   (cond
    ((null? tup) 0)
    (else (plus (car tup)
           (addtup (cdr tup)))))))
 (define multiply
  (lambda (n m)
   (cond
    ((zero? m) 0)
    (else (plus n (multiply n (sub1 m)))))))
 (define tup+
  (lambda (tup1 tup2)
   (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else
     (cons (plus (car tup1) (car tup2))
           (tup+ (cdr tup1) (cdr tup2)))))))
 (define gt
  (lambda (n m)
   (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (gt (sub1 n) (sub1 m))))))
 (define lt
  (lambda (n m)
   (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (lt (sub1 n) (sub1 m))))))
 (define equal
  (lambda (n m)
   (cond
    ((gt n m) #f)
    ((lt n m) #f)
    (else #t))))
 (define power
  (lambda (n m)
   (cond
    ((zero? m) 1)
    (else (multiply n (power n (sub1 m)))))))
 (define divide
  (lambda (n m)
   (cond
    ((lt n m) 0)
    (else (add1 (divide (minus n m) m))))))
 (define length
  (lambda (lat)
   (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat)))))))
 (define pick
  (lambda (n lat)
   (cond
    ((one? n) (car lat))
    (else (pick (sub1 n)
            (cdr lat))))))
 (define rempick
  (lambda (n lat)
   (cond
    ((one? n) (cdr lat))
    (else (cons (car lat)
           (rempick (sub1 n)
            (cdr lat)))))))
 (define no-nums
  (lambda (lat)
   (cond
    ((null? lat) quote())
    ((number? (car lat))
     (no-nums (cdr lat)))
    (else (cons (car lat)
           (no-nums
            (cdr lat)))))))
 (define all-nums
  (lambda (lat)
   (cond
    ((null? lat) quote())
    ((number? (car lat))
     (cons (car lat)
      (all-nums (cdr lat))))
    (else (all-nums (cdr lat))))))
 (define eqan?
  (lambda (a1 a2)
   (cond
    ((and (number? a1) (number? a2))
     (equal a1 a2))
    ((or (number? a1) (number? a2))
     #f)
    (else (eq? a1 a2)))))
 (define occur
  (lambda (a lat)
   (cond
    ((null? lat) 0)
    ((eq? (car lat) a)
     (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat))))))
 (define one?
  (lambda (n)
   (= n 1)))
 (export nums)
)
