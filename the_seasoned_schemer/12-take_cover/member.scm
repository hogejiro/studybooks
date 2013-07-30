(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module covers)
(print (member?-old 'ice '(salad greens with pears brie cheese frozen yogurt)))
; #f
(print (member? 'ice '(salad greens with pears brie cheese frozen yogurt)))
; #f
