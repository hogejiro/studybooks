(define-module shows
 (define member
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    (else (or (eq? a (car lat))
     (member a (cdr lat)))))))
 (define two-in-a-row-first-a?
  (lambda (lat)
   (cond
    ((null? lat) #f)
    (else
     (or (is-first? (car lat) (cdr lat))
      (two-in-a-row-first-a? (cdr lat)))))))
 (define is-first?
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    (else (eq? (car lat) a)))))
 (define two-in-a-row-first-b?
  (lambda (lat)
   (cond
    ((null? lat) #f)
    (else
     (is-first-b? (car lat) (cdr lat))))))
 (define is-first-b?
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
           (two-in-a-row-first-b? lat))))))
 (define two-in-a-row-b?
  (lambda (preceeding lat)
   (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) preceeding)
           (two-in-a-row-b? (car lat)
            (cdr lat)))))))
 (define two-in-a-row?
  (lambda (lat)
   (cond
    ((null? lat) #f)
    (else (two-in-a-row-b? (car lat)
           (cdr lat))))))
 (define sum-of-prefixes-b
  (lambda (sonssf tup)
   (cond
    ((null? tup) (quote ()))
    (else (cons (+ sonssf (car tup))
           (sum-of-prefixes-b
            (+ sonssf (car tup))
            (cdr tup)))))))
 (define sum-of-prefixes
  (lambda (tup)
   (sum-of-prefixes-b 0 tup)))
 (define pick
  (lambda (n lat)
   (cond
    ((= n 1) (car lat))
    (else (pick (- n 1) (cdr lat))))))
 (define scramble-b
  (lambda (tup rev-pre)
   (cond
    ((null? tup) (quote ()))
    (else (cons (pick (car tup)
                 (cons (car tup) rev-pre))
           (scramble-b (cdr tup)
            (cons (car tup) rev-pre)))))))
 (define scramble
  (lambda (tup)
   (scramble-b tup (quote ()))))
 (export shows)
)
