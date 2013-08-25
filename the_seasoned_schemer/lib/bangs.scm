(define-module bangs
 (define sweet-tooth
  (lambda (food)
   (cons food
    (cons (quote cake)
     (quote ())))))
 (define last (quote angelfood))
 (define sweet-toothL
  (lambda (food)
   (set! last food)
   (cons food
    (cons (quote cake)
     (quote ())))))
 (define ingredients (quote ()))
 (define sweet-toothR
  (lambda (food)
   (set! ingredients
    (cons food ingredients))
   (cons food
    (cons (quote cake)
     (quote ())))))
 (define sub1
  (lambda (n)
   (- n 1)))
 (define deep-a
  (lambda (m)
   (cond
    ((zero? m) (quote pizza))
    (else (cons (deep-a (sub1 m))
           (quote ()))))))
 (define Rs (quote ()))
 (define Ns (quote ()))
 (define deepR
  (lambda (n)
   (let ((result (deep-a n)))
    (set! Rs (cons result Rs))
    (set! Ns (cons n Ns))
    result)))
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
 (define member?
  (lambda (a lat)
   (letrec
    ((M (lambda (lat)
         (cond
          ((null? lat) #f)
          (else (or (eq? (car lat) a)
                 (M (cdr lat))))))))
    (M lat))))
 (define deepM-a
  (lambda (n)
   (if (member? n Ns)
    (find n Ns Rs)
    (deepR n))))
 (define deepM-b
  (lambda (n)
   (if (member? n Ns)
    (find n Ns Rs)
    (let ((result (deep-a n)))
     (set! Rs (cons result Rs))
     (set! Ns (cons n Ns))
     result))))
 (define deep
  (lambda (m)
   (cond
    ((zero? m) (quote pizza))
    (else (cons (deep (sub1 m))
           (quote ()))))))
 (define deepM-c
  (lambda (n)
   (if (member? n Ns)
    (find n Ns Rs)
    (let ((result (deep-b n)))
     (set! Rs (cons result Rs))
     (set! Ns (cons n Ns))
     result))))
 (define deepM-d
  (let ((Rs (quote ()))
        (Ns (quote ())))
   (lambda (n)
    (if (member? n Ns)
     (find n Ns Rs)
     (let ((result (deep-b n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))))
 (define atom?
  (lambda (a)
   (and (not (null? a))
    (not (pair? a)))))
 (define deepM-e
  (let ((Rs (quote ()))
        (Ns (quote ())))
   (lambda (n)
    (if (atom? (find n Ns Rs))
     (let ((result (deep-b n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
       result)
     (find n Ns Rs)))))
 (define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
   (lambda (n)
    (let ((exists (find n Ns Rs)))
     (if (atom? exists)
      (let ((result (deep n)))
       (set! Rs (cons result Rs))
       (set! Ns (cons n Ns))
        result)
      exists)))))
 (define add1
  (lambda (n)
   (+ n 1)))
 (define length-old
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (length-old (cdr l)))))))
 (define length-a
  (let ((h (lambda (l) 0)))
   (set! h
    (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (h (cdr l)))))))
   h))
 (define L
  (lambda (length)
   (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l))))))))
 (define length-b
  (let ((h (lambda (l) 0)))
   (set! h
    (L (lambda (arg) (h arg))))
   h))
 (define Y!
  (lambda (L)
   (let ((h (lambda (l) (quote ()))))
    (set! h
     (L (lambda (arg) (h arg))))
    h)))
 (define Y-bang
  (lambda (f)
   (letrec
    ((h (f (lambda (arg) (h arg)))))
    h)))
 (define length-c (Y! L))
 (define max
  (lambda (n m)
   ((> n m) n m)))
 (define D
  (lambda (depth*)
   (lambda (s)
    (cond
     ((null? s) 1)
     ((atom? (car s))
      (depth* (cdr s)))
     (else
      (max
       (add1 (depth* (car s)))
       (depth* (cdr s))))))))
 (define depth* (Y! D))
 (define Y
  (lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
     (le (lambda (x) ((f f) x)))))))
 (define biz
  (let ((x 0))
   (lambda (f)
    (set! x (add1 x))
    (lambda (a)
     (if (= a x)
      0
      (f a))))))
 (export bangs)
)
