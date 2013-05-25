(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (((lambda (length)
          (lambda (l)
           (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
         ((lambda (length)
           (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
          ((lambda (length)
            (lambda (l)
             (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))
           eternity))) '(a b)))
; 2
