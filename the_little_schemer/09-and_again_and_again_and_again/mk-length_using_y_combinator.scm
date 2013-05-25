(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print ((Y
        (lambda (length)
         (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1 (length (cdr l))))))))
        '(a b c d)))
; 4
