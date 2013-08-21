(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module names)
(print (leftmost-old '(((a) b) (c d))))
; a
; (print (leftmost-old '(((() a) ()))))
; [error] no answer.
(print (leftmost-a '(((() a) ()))))
; a
(print (leftmost-b '(((() a) ()))))
; a
(print (leftmost-c '(((() a) ()))))
; a
(print (leftmost '(((() a) ()))))
; a
