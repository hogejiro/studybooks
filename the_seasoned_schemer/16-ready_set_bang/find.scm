(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(deepR 3)
(deepR 5)
(deepR 3)
(print (find 3 Ns Rs))
; (((pizza)))
(print (find 5 Ns Rs))
; (((((pizza)))))
(print (find 7 Ns Rs))
; #f
