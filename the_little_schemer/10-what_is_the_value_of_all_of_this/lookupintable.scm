(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module values)
(print (lookup-in-table 'entree '(((entree dessert) (spaghetti spumoni)) ((appetizer entree beverage) (food tastes good))) (lambda (x) x)))
; spaghetti
