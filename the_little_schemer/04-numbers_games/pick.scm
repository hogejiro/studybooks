(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module nums)
(print (pick 4 '(lasagna spagetti ravioli macaroni meatball)))
; macaroni 
