(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module values)
(print (lookup-in-entry 'entree '((appetizer entree beverage) (food tastes good)) (lambda (x) x)))
; tastes
(print (lookup-in-entry 'dessert '((appetizer entree beverage) (food tastes good)) (lambda (x) x)))
; dessert
