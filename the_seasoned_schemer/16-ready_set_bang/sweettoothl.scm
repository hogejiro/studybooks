(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(print "last: "last)
; last: angelfood
(print (sweet-toothL 'chocolate))
; (chocolate cake)
(print "last: "last)
; last: chocolate
(print (sweet-toothL 'fruit))
; (fruit cake)
(print "last: "last)
; last: fruit
