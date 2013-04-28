(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (occur 'cup ' (coffee cup tea cup and hick cup)))
; 3
