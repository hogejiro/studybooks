(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module agains)
(print (member? 'tea '(coffee tea or milk)))
; #t
(print (member? 'poached '(fried eggs and scrambled eggs)))
; #f
