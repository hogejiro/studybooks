(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module stars)
(print (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))))
; 5
