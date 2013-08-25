(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module changes)
(set-counter 0)
(print (rember1*C 'noodles '((food) more (food))))
; ((food) more (food))
(print (counter))
; 0
(set-counter 0)
(print (consC-b (consC-b 'food (quote ()))
 (consC-b 'more
  (consC-b (consC-b 'food (quote ()))
   (quote ())))))
; ((food) more (food))
(print (counter))
; 5
