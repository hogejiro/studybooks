(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print (((lambda (mk-length)
         (mk-length mk-length))
        (lambda (mk-length)
         (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1
                  ((mk-length mk-length) (cdr l))))))))
        '(a b c d)))
; 3
