(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)))
; #t 
