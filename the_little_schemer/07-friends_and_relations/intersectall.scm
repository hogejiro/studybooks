(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module relations)
(print (intersectall '((a b c) (c a d e) (e f g h a b))))
; (a)
(print (intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with lots of apples))))
; (6 and)