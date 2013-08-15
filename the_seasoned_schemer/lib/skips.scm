(define-module skips
 (define member?
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
           (member? a (cdr lat)))))))
 (define intersect-old
  (lambda (set1 set2)
   (cond
    ((null? set1) (quote ()))
    ((member? (car set1) set2)
     (cons (car set1)
      (intersect-old (cdr set1) set2)))
    (else (intersect-old (cdr set1) set2)))))
 (define intersect-a
  (lambda (set1 set2)
   (letrec
    ((I (lambda (set)
         (cond
          ((null? set) (quote ()))
          ((member? (car set) set2)
           (cons (car set)
            (I (cdr set))))
          (else (I (cdr set)))))))
    (I set1))))
 (define intersectall-old
  (lambda (lset)
   (cond
    ((null? lset) (quote ()))
    ((null? (cdr lset)) (car lset))
    (else (intersect-old (car lset)
           (intersectall-old (cdr lset)))))))
 (define intersectall-a
  (lambda (lset)
   (letrec
    ((A (lambda (lset)
         (cond
          ((null? (cdr lset)) (car lset))
          (else (intersect-a (car lset)
                 (A (cdr lset))))))))
    (cond
     ((null? lset) (quote ()))
     (else (A lset))))))
 (define intersectall-b
  (lambda (lset)
   (call-with-current-continuation
    (lambda (hop)
     (letrec
      ((A (lambda (lset)
           (cond
            ((null? (car lset))
             (hop (quote ())))
            ((null? (cdr lset))
             (car lset))
            (else
             (intersect-a (car lset)
              (A (cdr lset))))))))
      (cond
       ((null? lset) (quote ()))
       (else (A lset))))))))
 (define intersect
  (lambda (set1 set2)
   (letrec
    ((I (lambda (set1)
         (cond
          ((null? set1) (quote ()))
          ((member? (car set1)
           set2)
           (cons (car set1)
            (I (cdr set1))))
          (else (I (cdr set1)))))))
    (cond
     ((null? set2) (quote ()))
     (else (I set1))))))
 (define intersectall
  (lambda (lset)
   (call-with-current-continuation
    (lambda (hop)
     (letrec
      ((A (lambda (lset)
           (cond
            ((null? (car lset))
             (hop (quote ())))
            ((null? (cdr lset))
             (car lset))
            (else (I (car lset)
                   (A (cdr lset)))))))
       (I (lambda (s1 s2)
           (letrec
            ((J (lambda (s1)
                 (cond
                  ((null? s1) (quote ()))
                  ((member? (car s1) s2)
                   (cons (car s1)
                    (J (cdr s1))))
                  (else (J (cdr s1)))))))
             (cond
              ((null? s2) (hop (quote ())))
              (else (J s1)))))))
    (cond
     ((null? lset) (quote ()))
     (else (A lset))))))))
 (define rember
  (lambda (a lat)
   (letrec
    ((R (lambda (lat)
         (cond
          ((null? lat) (quote ()))
          ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat)
                 (R (cdr lat))))))))
    (R lat))))
 (define rember-beyond-first
  (lambda (a lat)
   (letrec
    ((R (lambda (lat)
         (cond
          ((null? lat) (quote ()))
          ((eq? (car lat) a) (quote ()))
          (else (cons (car lat)
                 (R (cdr lat))))))))
    (R lat))))
 (define rember-upto-last
  (lambda (a lat)
   (call-with-current-continuation
    (lambda (skip)
     (letrec
      ((R (lambda (lat)
           (cond
            ((null? lat) (quote ()))
            ((eq? (car lat) a)
             (skip (R (cdr lat))))
            (else (cons (car lat)
                   (R (cdr lat))))))))
    (R lat))))))
 (export skips)
)
