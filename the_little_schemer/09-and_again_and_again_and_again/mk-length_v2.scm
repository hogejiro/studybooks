(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (((lambda (mk-length)
         (mk-length mk-length))
        (lambda (mk-length)
         ((lambda (length)
           (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
          (lambda (x)
           ((mk-length mk-length) x)))))
        '(a b c d)))
; 4
