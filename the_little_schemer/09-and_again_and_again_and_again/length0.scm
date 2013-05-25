(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module againandagains)
(print((lambda (l)
 (cond
  ((null? l) 0)
  (else (add1 (eternity (cdr l))))))
       '()))
; 0
