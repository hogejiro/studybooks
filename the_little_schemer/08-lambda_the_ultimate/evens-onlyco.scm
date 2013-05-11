(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module lambda)
(define the-last-friend
 (lambda (newl product sum)
  (cons sum
   (cons product
    newl))))
(print (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend))
; (38 1920 (2 8) 10 (() 6) 2)
