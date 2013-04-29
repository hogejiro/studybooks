(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module stars)
(print (member* 'chips '((potato) (chips ((with) fish)) (chips))))
; #t
