(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module conses)
(print (firsts 
        '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant))))
; (apple plum grape bean)
(print (firsts
        '(((five plums) four)
            (eleven green oranges)
            ((no) more))))
; ((five plums) eleven (no))
