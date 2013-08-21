(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module names)
(print (rember1*-a 'salad '((Swedish rye) (French (mustard salad turkey)) salad)))
; #t
(print (rember1*-b 'salad '((Swedish rye) (French (mustard salad turkey)) salad)))
; #t
(print (rember1*-c 'salad '((Swedish rye) (French (mustard salad turkey)) salad)))
; #t
(print (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad)))
; #t
