(define-module changes
 (define find
  (lambda (n Ns Rs)
   (letrec
    ((A (lambda (ns rs)
         (cond
          ((null? ns) #f)
          ((= (car ns) n) (car rs))
          (else
           (A (cdr ns) (cdr rs)))))))
    (A Ns Rs))))
 (define atom?
  (lambda (a)
   (and (not (null? a))
    (not (pair? a)))))
 (define sub1
  (lambda (n)
   (- n 1)))
 (define deepM-a
  (let ((Rs (quote ()))
        (Ns (quote ())))
   (lambda (n)
    (let ((exists (find n Ns Rs)))
     (if (atom? exists)
      (let ((result
             (if (zero? n)
              (quote pizza)
              (cons (deepM-a (sub1 n))
               (quote ())))))
       (set! Rs (cons result Rs))
       (set! Ns (cons n Ns))
        result)
      exists)))))
 (define add1
  (lambda (n)
   (+ n 1)))
 (define deep-a
  (lambda (m)
   (if (zero? m)
    (quote pizza)
    (consC-a (deep-a (sub1 m))
     (quote ())))))
 (define counter 0)
 (define set-counter (lambda ()))
 (define consC-a
  (let ((N 0))
   (set! counter
    (lambda ()
     N))
   (lambda (x y)
    (set! N (add1 N))
    (cons x y))))
 (define supercounter
  (lambda (f)
   (letrec
    ((S (lambda (n)
         (if (zero? n)
          (f n)
          (let ()
           (f n)
           (S (sub1 n)))))))
    (S 1000)
    (counter))))
 (define consC-b
  (let ((N 0))
   (set! counter
    (lambda ()
     N))
   (set! set-counter
    (lambda (x)
     (set! N x)))
   (lambda (x y)
    (set! N (add1 N))
    (cons x y)))) 
 (define deep-b
  (lambda (m)
   (if (zero? m)
    (quote pizza)
    (consC-b (deep-b (sub1 m))
     (quote ())))))
 (define deepM-b
  (let ((Rs (quote ()))
        (Ns (quote ())))
   (lambda (n)
    (let ((exists (find n Ns Rs)))
     (if (atom? exists)
      (let ((result
             (if (zero? n)
              (quote pizza)
              (consC-b
               (deepM-b (sub1 n))
               (quote ())))))
       (set! Rs (cons result Rs))
       (set! Ns (cons n Ns))
        result)
      exists)))))
 (define rember1*
  (lambda (a l)
   (letrec
    ((R (lambda (l oh)
         (cond
          ((null? l)
           (oh (quote no)))
          ((atom? (car l))
           (if (eq? (car l) a)
            (cdr l)
            (cons (car l)
             (R (cdr l) oh))))
          (else
           (let ((new-car
                  (let/cc oh
                   (R (car l)
                    oh))))
            (if (atom? new-car)
             (cons (car l)
              (R (cdr l) oh))
             (cons new-car
              (cdr l)))))))))
    (let ((new-l (let/cc oh (R l oh))))
     (if (atom? new-l)
      l
      new-l)))))
 (define rember1*C
  (lambda (a l)
   (letrec
    ((R (lambda (l oh)
         (cond
          ((null? l)
           (oh (quote no)))
          ((atom? (car l))
           (if (eq? (car l) a)
            (cdr l)
            (consC-b (car l)
             (R (cdr l) oh))))
          (else
           (let ((new-car
                  (let/cc oh
                   (R (car l)
                    oh))))
            (if (atom? new-car)
             (consC-b (car l)
              (R (cdr l) oh))
             (consC-b new-car
              (cdr l)))))))))
    (let ((new-l (let/cc oh (R l oh))))
     (if (atom? new-l)
      l
      new-l)))))
 (define eqlist?
  (lambda (l1 l2)
   (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))
 (define rember1*C2
  (lambda (a l)
   (letrec
    ((R (lambda (l)
         (cond
          ((null? l) (quote ()))
          ((atom? (car l))
           (cond
            ((eq? (car l) a) (cdr l))
            (else (consC-b (car l)
                   (R (cdr l))))))
          (else
           (let ((av (R (car l))))
            (cond
             ((eqlist? (car l) av)
              (consC-b (car l)
               (R (cdr l))))
             (else (consC-b av
                    (cdr l))))))))))
    (R l))))
 (export changes)
)
