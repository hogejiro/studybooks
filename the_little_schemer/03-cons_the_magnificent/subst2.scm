(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module conses)
(print (subst2 'vanilla 'chocolate 'banana '(banana ice creamwith chocolate topping)))
; (vanilla ice creamwith chocolate topping)