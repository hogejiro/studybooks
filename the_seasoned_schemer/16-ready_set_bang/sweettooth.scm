(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(print (sweet-tooth 'chocolate))
; (chocolate cake)
(print "last: "last)
; last: angelfood
(print (sweet-tooth 'fruit))
; (fruit cake)
(print "last: "last)
; last: angelfood
