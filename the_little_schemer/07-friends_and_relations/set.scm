(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (set? '(apple peaches apple plum)))
; #f
(print (set? '(apples peaches pears plums)))
; #t
