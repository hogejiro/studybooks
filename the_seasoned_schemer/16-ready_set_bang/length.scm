(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(print (length-old '(a b c d)))
; 4
(print (length-a '(a b c d)))
; 4
(print (length-b '(a b c d)))
; 4
(print (length-c '(a b c d)))
; 4
