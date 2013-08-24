(define-module differences
 (define x
  (cons (quote chicago)
   (cons (quote pizza)
    (quote ()))))
 (define gourmet
  (lambda (food)
   (cons food
    (cons x (quote ())))))
 (define gourmand
  (lambda (food)
   (set! x food)
   (cons food
    (cons x
     (quote ())))))
 (define diner
  (lambda (food)
   (cons (quote milkshake)
    (cons food
     (quote ())))))
 (define dinerR
  (lambda (food)
   (set! x food)
   (cons (quote milkshake)
    (cons food
     (quote ())))))
 (define omnivore
  (let ((x (quote minestrone)))
   (lambda (food)
    (set! x food)
    (cons food
     (cons x
      (quote ()))))))
 (define gobbler
  (let ((x (quote minestrone)))
   (lambda (food)
    (set! x food)
    (cons food
     (cons x
      (quote ()))))))
 (define nibbler
  (lambda (food)
   (let ((x (quote donut)))
    (set! x food)
    (cons food
     (cons x
      (quote ()))))))
 (define adder-no-closure
  (lambda (n)
   (let ((cnt 0))
    (set! cnt (+ cnt n))
    (print cnt))))
 (define adder-closure
  (let ((cnt 0))
   (lambda (n)
    (set! cnt (+ cnt n))
    (print cnt))))
 (define food (quote none))
 (define glutton
  (lambda (x)
   (set! food x)
   (cons (quote more)
    (cons x
     (cons (quote more)
      (cons x
       (quote ())))))))
 (define chez-nous
  (lambda ()
   (let ((a food))
    (set! food x)
    (set! x a))))
 (export differences)
)
