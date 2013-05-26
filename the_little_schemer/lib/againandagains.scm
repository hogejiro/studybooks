(define-module againandagains
 (define pick
  (lambda (n lat)
   (cond
    ((= n 1) (car lat))
    (else (pick (- n 1) (cdr lat))))))
 (define keep-looking
  (lambda (a sorn lat)
   (cond
    ((number? sorn)
     (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a)))))
 (define looking
  (lambda (a lat)
   (keep-looking a (pick 1 lat) lat)))
 (define eternity
  (lambda (x)
   (eternity x)))
 (define first
  (lambda (p) 
   (car p)))
 (define second
  (lambda (p)
   (car (cdr p))))
 (define build
  (lambda (a1 a2)
   (cons a1
    (cons a2 (quote())))))
 (define shift
  (lambda (pair)
   (build (first (first pair))
    (build (second (first pair))
     (second pair)))))
 (define atom?
  (lambda (x)
   (and (not (null? x)) (not (pair? x)))))
 (define a-pair?
  (lambda (x)
   (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))
 (define align
  (lambda (para)
   (cond
    ((atom? para) para)
    ((a-pair? (first para))
     (align (shift para)))
    (else (build (first para)
           (align (second para)))))))
 (define length*
  (lambda (para)
   (cond
    ((atom? para) 1)
    (else
     (+ (length* (first para))
      (length* (second para)))))))
 (define weight*
  (lambda (pora)
   (cond
    ((atom? pora) 1)
    (else
     (+ (* (weight* (first pora)) 2)
      (weight* (second pora)))))))
 (define revpair
  (lambda (pair)
   (build (second pair) (first pair))))
 (define shuffle
  (lambda (pora)
   (cond
    ((atom? pora) pora)
    ((a-pair? (first pora))
     (shuffle (revpair pora)))
    (else (build (first pora)
           (shuffle (second pora)))))))
 (define C
  (lambda (n)
   (cond
    ((= n 1) 1)
    (else
     (cond
      ((even? n) (C (/ n 2)))
      (else (C (+ (* 3 n) 1))))))))
 (define A
  (lambda (n m)
   (cond
    ((= n 0) (+ m 1))
    ((= m 0) (A (- n 1) 1))
    (else (A (- n 1)
           (A n (- m 1)))))))
 (define length
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (+ (length (cdr l)) 1)))))
 (define add1
  (lambda (n)
   (+ n 1)))
 (define Y
  (lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
     (le (lambda (x) ((f f) x)))))))
 (export againandagains)
)
