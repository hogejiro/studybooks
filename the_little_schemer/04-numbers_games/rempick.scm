(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (rempick 3 '(hotdogs with hot mustard)))
; (hotdogs with mustard)