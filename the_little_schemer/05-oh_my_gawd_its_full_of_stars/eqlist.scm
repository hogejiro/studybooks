(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module stars)
(print (eqlist? '(strawberry ice cream) '(strawberry ice cream)))
; #t
(print (eqlist? '(strawberry ice cream) '(strawberry cream ice)))
; #f
(print (eqlist? '(banana ((split))) '((banana) (split))))
; #f
(print (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))))
; #f
(print (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))
; #t
