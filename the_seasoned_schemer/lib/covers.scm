(define-module covers
 (define multirember-y
  (lambda (a lat)
   ((Y (lambda (mr)
        (lambda (lat)
         (cond
          ((null? lat) (quote ()))
          ((eq? a (car lat))
           (mr (cdr lat)))
          (else (cons (car lat)
                 (mr (cdr lat))))))))
    lat)))
 (define Y
  (lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
     (le (lambda (x) ((f f) x)))))))
 (define multirember
  (lambda (a lat)
   ((letrec
     ((mr (lambda (lat)
           (cond
            ((null? lat) (quote ()))
            ((eq? a (car lat))
             (mr (cdr lat)))
            (else
             (cons (car lat)
              (mr (cdr lat))))))))
     mr)
    lat)))
 (define multirember-dash
  (lambda (a lat)
   (letrec
    ((mr (lambda (lat)
          (cond
           ((null? lat) (quote ()))
           ((eq? a (car lat))
            (mr (cdr lat)))
           (else
            (cons (car lat)
             (mr (cdr lat))))))))
    (mr lat))))
 (define rember-f
  (lambda (test?)
   (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
            ((rember-f test?) a
             (cdr l))))))))
 (define multirember-f-old
  (lambda (test?)
   (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat) a)
      ((multirember-f-old test?) a
       (cdr lat)))
     (else (cons (car lat)
            ((multirember-f-old test?) a
             (cdr lat))))))))
 (define multirember-f
  (lambda (test?)
   (letrec
    ((m-f (lambda (a lat)
           (cond
            ((null? lat) (quote ()))
            ((test? (car lat) a)
             (m-f a (cdr lat)))
            (else
             (cons (car lat)
              (m-f a (cdr lat))))))))
    m-f)))
 (define member?-old
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    ((eq? (car lat) a) #t)
    (else (member?-old a (cdr lat))))))
 (define member?
  (lambda (a lat)
   ((letrec
     ((yes? (lambda (l)
             (cond
              ((null? l) #f)
              ((eq? (car l) a) #t)
              (else (yes? (cdr l)))))))
     yes?)
    lat)))
 (define union-old
  (lambda (set1 set2)
   (cond
    ((null? set1) set2)
    ((member? (car set1) set2)
     (union-old (cdr set1) set2))
    (else (cons (car set1)
           (union-old (cdr set1) set2))))))
 (define union-a
  (lambda (set1 set2)
   (letrec
    ((U (lambda (set)
         (cond
          ((null? set) set2)
          ((member? (car set) set2)
           (U (cdr set)))
          (else (cons (car set)
                 (U (cdr set))))))))
    (U set1))))
 (define union-b
  (lambda (set1 set2)
   (letrec
    ((U (lambda (set)
         (cond
          ((null? set) set2)
          ((M? (car set) set2)
           (U (cdr set)))
          (else (cons (car set)
                 (U (cdr set)))))))
     (M?
      (lambda (a lat)
       (cond
        ((null? lat) #f)
        ((eq? (car lat) a) #t)
        (else (M? a (cdr lat)))))))
    (U set1))))
 (define union
  (lambda (set1 set2)
   (letrec
    ((U (lambda (set)
         (cond
          ((null? set) set2)
          ((M? (car set) set2)
           (U (cdr set)))
          (else (cons (car set)
                 (U (cdr set)))))))
     (M?
      (lambda (a lat)
       (letrec
        ((N? (lambda (lat)
              (cond
               ((null? lat) #f)
               ((eq? (car lat) a) #t)
               (else (N? (cdr lat)))))))
        (N? lat)))))
    (U set1))))
 (define two-in-a-row?-a
  (lambda (lat)
   (letrec
    ((W (lambda (a lat)
         (cond
          ((null? lat) #f)
          (else (or (eq? (car lat) a)
                 (W (car lat) (cdr lat))))))))
     (cond
      ((null? lat) #f)
      (else (W (car lat) (cdr lat)))))))
 (define two-in-a-row?-b
   (letrec
    ((W (lambda (a lat)
         (cond
          ((null? lat) #f)
          (else (or (eq? (car lat) a)
                 (W (car lat) (cdr lat))))))))
     (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))
 (define sum-of-prefixes
  (lambda (tup)
   (letrec
    ((S (lambda (sss tup)
         (cond
          ((null? tup) (quote ()))
          (else
           (cons (+ sss (car tup))
            (S (+ sss (car tup))
             (cdr tup))))))))
    (S 0 tup))))
 (define pick
  (lambda (n lat)
   (cond
    ((= n 1) (car lat))
    (else (pick (- n 1) (cdr lat))))))
 (define scramble
  (lambda (tup)
   (letrec
    ((P (lambda (tup rp)
         (cond
          ((null? tup) (quote ()))
          (else (cons (pick (car tup)
                       (cons (car tup) rp))
                 (P (cdr tup)
                  (cons (car tup) rp))))))))
    (P tup (quote ())))))
 (define scramble-b
   (letrec
    ((P (lambda (tup rp)
         (cond
          ((null? tup) (quote ()))
          (else (cons (pick (car tup)
                       (cons (car tup) rp))
                 (P (cdr tup)
                  (cons (car tup) rp))))))))
    (lambda (tup)
     (P tup (quote ())))))
 (export covers)
)
