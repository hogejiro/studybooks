(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)))
; #t
(print (subset? '(4 pounds of houseradish) '(four pounds chicken and5 ounces horseradish)))
; #f
