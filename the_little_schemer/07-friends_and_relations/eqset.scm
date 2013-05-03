(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (eqset? '(6 large chickens with wings) '(6 chickens with large wings)))
; #t
