(define-module changesames
 (define kar
  (lambda (c)
   (c (lambda (s a d) a))))
 (define kdr
  (lambda (c)
   (c (lambda (s a d) d))))
 (define bons
  (lambda (kar)
   (let ((kdr (quote ())))
    (lambda (selector)
     (selector
      (lambda (x) (set! kdr x))
      kar
      kdr)))))
 (define set-kdr
  (lambda (c x)
   ((c (lambda (s a d) s)) x)))
 (define kons
  (lambda (a d)
   (let ((c (bons a)))
    (set-kdr c d)
    c)))
 (define sub1
  (lambda (n)
   (- n 1)))
 (define lots
  (lambda (m)
   (cond
    ((zero? m) (quote ()))
    (else (kons (quote egg)
           (lots (sub1 m)))))))
 (define add1
  (lambda (n)
   (+ n 1)))
 (define lenkth
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (lenkth (kdr l)))))))
 (define add-at-end-too
  (lambda (l)
   (letrec
    ((A (lambda (ls)
         (cond
          ((null? (kdr ls))
           (set-kdr ls
            (kons (quote egg)
             (quote ()))))
          (else (A (kdr ls)))))))
    (A l)
    l)))
 (define kounter 0)
 (define set-kounter (lambda ()))
 (define konsC
  (let ((N 0))
   (set! kounter
    (lambda ()
     N))
   (set! set-kounter
    (lambda (x)
     (set! N x)))
   (lambda (x y)
    (set! N (add1 N))
    (kons x y))))
 (define add-at-end
  (lambda (l)
   (cond
    ((null? (kdr l))
     (konsC (kar l)
      (kons (quote egg)
       (quote ()))))
    (else (konsC (kar l)
           (add-at-end (kdr l)))))))
 (define lotsC
  (lambda (m)
   (cond
    ((zero? m) (quote ()))
    (else (konsC (quote egg)
           (lotsC (sub1 m)))))))
 (define add-at-end-tooC
  (lambda (l)
   (letrec
    ((A (lambda (ls)
         (cond
          ((null? (kdr ls))
           (set-kdr ls
            (konsC (quote egg)
             (quote ()))))
          (else (A (kdr ls)))))))
    (A l)
    l)))
 (define same?
  (lambda (c1 c2)
   (let ((t1 (kdr c1))
         (t2 (kdr c2)))
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ((v (= (kdr c1) (kdr c2))))
     (set-kdr c1 t1)
     (set-kdr c2 t2)
     v))))
 (export changesames) 
)
