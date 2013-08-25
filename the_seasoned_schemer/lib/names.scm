(define-module names
 (define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))
 (define leftmost-old
  (lambda (l)
   (cond
    ((atom? (car l)) (car l))
    (else (leftmost-old (car l))))))
 (define leftmost-a
  (lambda (l)
   (cond
    ((null? l) (quote ()))
    ((atom? (car l)) (car l))
    (else (cond
           ((atom? (leftmost-a (car l)))
            (leftmost-a (car l)))
           (else (leftmost-a (cdr l))))))))
 (define leftmost-b
  (lambda (l)
   (cond
    ((null? l) (quote ()))
    ((atom? (car l)) (car l))
    (else
     (let ((a (leftmost-b (car l))))
      (cond
       ((atom? a) a)
       (else (leftmost-b (cdr l)))))))))
 (define eqlist?
  (lambda (l1 l2)
   (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))
 (define rember1*-a
  (lambda (a l)
   (cond
    ((null? l) (quote ()))
    ((atom? (car l))
     (cond
      ((eq? (car l) a) (cdr l))
      (else (cons (car l)
             (rember1*-a a (cdr l))))))
    (else
     (cond
      ((eqlist?
        (rember1*-a a (car l))
        (car l))
       (cons (car l)
        (rember1*-a a (cdr l))))
      (else (cons (rember1*-a a (car l))
             (cdr l))))))))
 (define rember1*-b
  (lambda (a l)
   (letrec
    ((R (lambda (l)
         (cond
          ((null? l) (quote ()))
          ((atom? (car l))
           (cond
            ((eq? (car l) a) (cdr l))
            (else (cons (car l)
                   (R (cdr l))))))
          (else
           (cond
            ((eqlist?
              (R (car l))
              (car l))
             (cons (car l)
              (R (cdr l))))
            (else (cons (R (car l))
                   (cdr l)))))))))
    (R l))))
 (define rember1*-c
  (lambda (a l)
   (letrec
    ((R (lambda (l)
         (cond
          ((null? l) (quote ()))
          ((atom? (car l))
           (cond
            ((eq? (car l) a) (cdr l))
            (else (cons (car l)
                   (R (cdr l))))))
          (else
           (let ((av (R (car l))))
            (cond
             ((eqlist? (car l) av)
              (cons (car l)
               (R (cdr l))))
             (else (cons av
                    (cdr l))))))))))
    (R l))))
 (define add1
  (lambda (n)
   (+ 1 n)))
 (define depth*-a
  (lambda (l)
   (cond
    ((null? l) 1)
    ((atom? (car l))
     (depth*-a (cdr l)))
    (else
     (cond
      ((> (depth*-a (cdr l))
        (add1 (depth*-a (car l))))
       (depth*-a (cdr l)))
      (else
       (add1 (depth*-a (car l)))))))))
 (define depth*-b
  (lambda (l)
   (let ((a (add1 (depth*-b (car l))))
         (d (depth*-b (cdr l))))
    (cond
     ((null? l) 1)
     ((atom? (car l)) d)
     (else (cond
       ((> d a) d)
       (else a)))))))
 (define depth*-c
  (lambda (l)
   (cond
    ((null? l) 1)
    ((atom? (car l))
     (depth*-c (cdr l)))
    (else
     (let ((a (add1 (depth*-c (car l))))
           (d (depth*-c (cdr l))))
      (cond
       ((> d a) d)
       (else a)))))))
 (define depth*-d
  (lambda (l)
   (cond
    ((null? l) 1)
    (else
     (let ((d (depth*-d (cdr l))))
      (cond
       ((atom? (car l)) d)
       (else
        (let ((a (add1 (depth*-d (car l)))))
         (cond
          ((> d a) d)
          (else a))))))))))
 (define depth*-e
  (lambda (l)
   (cond
    ((null? l) 1)
    ((atom? (car l))
     (depth*-e (cdr l)))
    (else
     (let ((a (add1 (depth*-e (car l))))
           (d (depth*-e (cdr l))))
      (if (> d a) d a))))))
 (define max
  (lambda (n m)
   (if (> n m) n m)))
 (define depth*-f
  (lambda (l)
   (cond
    ((null? l) 1)
    ((atom? (car l))
     (depth*-f (cdr l)))
    (else
     (let ((a (add1 (depth*-f (car l))))
           (d (depth*-f (cdr l))))
      (max a d))))))
 (define depth*
  (lambda (l)
   (cond
    ((null? l) 1)
    ((atom? (car l))
     (depth* (cdr l)))
    (else (max
           (add1 (depth* (car l)))
           (depth* (cdr l)))))))
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
          (else
           (let ((rp (cons (car tup) rp)))
            (cons (pick (car tup) rp)
                  (P (cdr tup) rp))))))))
    (P tup (quote ())))))
 (define leftmost-c
  (lambda (l)
   (let/cc skip
    (lm l skip))))
 (define lm
  (lambda (l out)
   (cond
    ((null? l) (quote ()))
    ((atom? (car l)) (out (car l)))
    (else (let ()
           (lm (car l) out)
           (lm (cdr l) out))))))
 (define leftmost
  (lambda (l)
   (let/cc skip
    (letrec
     ((lm (lambda (l)
           (cond
            ((null? l) (quote ()))
            ((atom? (car l))
             (skip (car l)))
            (else
             (let ()
              (lm (car l))
              (lm (cdr l))))))))
     (lm l)))))
 (define rm-a
  (lambda (a l oh)
   (cond
    ((null? l) (oh (quote no)))
    ((atom? (car l))
     (if (eq? (car l) a)
      (cdr l)
      (cons (car l)
       (rm-a a (cdr l) oh))))
    (else
     (if (atom?
          (let/cc oh
           (rm-a a (car l) oh)))
         (cons (car l)
          (rm-a a (cdr l) oh))
         (cons (rm-a a (car l) 0)
          (cdr l)))))))
 (define rember1*-b
  (lambda (a l)
   (if (atom? (let/cc oh (rm-a a l oh)))
    l
    (rm-a a l (quote ())))))
 (define rember1*-c
  (lambda (a l)
   (let ((new-l (let/cc oh (rm-a a l oh))))
    (if (atom? new-l)
     l
     new-l))))
 (define rm-b
  (lambda (a l oh)
   (cond
    ((null? l) (oh (quote no)))
    ((atom? (car l))
     (if (eq? (car l) a)
      (cdr l)
      (cons (car l)
       (rm-b a (cdr l) oh))))
    (else
     (let ((new-car
            (let/cc oh
             (rm-b a (car l) oh))))
      (if (atom? new-car)
       (cons (car l)
          (rm-b a (cdr l) oh))
         (cons new-car (cdr l))))))))
 (define-macro (try x a b)
  (let ((success (gensym)))
   `(let/cc ,success
       (let/cc ,x
        (,success ,a))
       ,b)))
 (define rember1*
  (lambda (a l)
   (try oh (rm-b a l oh) l)))
 (define rm
  (lambda (a l oh)
   (cond
    ((null? l) (oh (quote no)))
    ((atom? (car l))
     (if (eq? (car l) a)
      (cdr l)
      (cons (car l)
       (rm a (cdr l) oh))))
    (else
     (try oh2
      (cons (rm a (car l) oh2)
       (cdr l))
      (cons (car l)
       (rm a (cdr l) oh)))))))
 (export names)
)
