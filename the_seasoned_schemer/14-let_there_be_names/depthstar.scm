(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module names)
(print (depth*-a '((pickled) peppers (peppers pickled))))
; 2
(print (depth*-a '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
; (print (depth*-b '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
(print (depth*-c '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
(print (depth*-d '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
(print (depth*-e '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
(print (depth*-f '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
(print (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)))
; 4
