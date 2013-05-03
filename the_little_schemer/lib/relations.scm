(define-module relations
 (define member?
  (lambda (a lat)
   (cond
    ((null? lat) #f)
    (else (or (equal? (car lat) a)
     (member? a (cdr lat)))))))
 (define set?
  (lambda (lat)
   (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat))))))
 (define multirember
  (lambda (a lat)
   (cond
    ((null? lat) quote())
    ((equal? (car lat) a)
     (multirember a (cdr lat)))
    (else (cons (car lat)
           (multirember a (cdr lat)))))))
 (define makeset
  (lambda (lat)
   (cond
    ((null? lat) quote())
    (else (cons (car lat)
           (makeset
            (multirember (car lat)
             (cdr lat))))))))
 (define subset?
  (lambda (set1 set2)
   (cond
    ((null? set1) #t)
    (else (and (member? (car set1) set2)
     (subset? (cdr set1) set2))))))
 (define eqset?
  (lambda (set1 set2)
   (and (subset? set1 set2)
    (subset? set1 set2))))
 (define intersect?
  (lambda (set1 set2)
   (cond
    ((null? set1) #f)
    (else (or (member? (car set1) set2)
     (intersect?
      (cdr set1) set2))))))
 (define intersect
  (lambda (set1 set2)
   (cond
    ((null? set1) quote())
    ((member? (car set1) set2)
     (cons (car set1)
      (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2)))))
 (define union
  (lambda (set1 set2)
   (cond
    ((null? set1) set2)
    ((member? (car set1) set2)
     (union (cdr set1) set2))
    (else (cons (car set1)
           (union (cdr set1) set2))))))
 (define except
  (lambda (set1 set2)
   (cond
    ((null? set1) quote())
    ((member? (car set1) set2)
     (except (cdr set1) set2))
    (else (cons (car set1)
           (except (cdr set1) set2))))))
 (define intersectall
  (lambda (l-set)
   (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set)
      (intersectall (cdr l-set)))))))
 (define atom?
  (lambda (x)
   (and (not (null? x))
    (not (pair? x)))))
 (define a-pair?
  (lambda (x)
   (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))
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
 (define third
  (lambda (l)
   (car (cdr (cdr l)))))
 (define firsts
  (lambda (l)
   (cond
    ((null? l) quote())
    (else (cons (car (car l)) 
           (firsts (cdr l)))))))
 (define fun?
  (lambda (rel)
   (set? (firsts rel))))
 (define revpair
  (lambda (pair)
   (build (second pair) (first pair))))
 (define revrel
  (lambda (rel)
   (cond
    ((null? rel) quote())
    (else (cons (revpair (car rel))
           (revrel (cdr rel)))))))
 (define seconds
  (lambda (l)
   (cond
    ((null? l) quote())
    (else (cons (car (cdr (car l)))
           (seconds (cdr l)))))))
 (define fullfun?
  (lambda (fun)
   (set? (seconds fun))))
 (export relations)
 (define one-to-one?
  (lambda (fun)
   (fun? (revrel fun))))
)
