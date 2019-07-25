(* 13.1 *) 
 
#use "sect13_1/count.ml" ;; 
count lst4 "A" ;; 
count lst4 "B" ;; 
 
(* 13.2 *) 
 
#use "sect13_2/map.ml" ;; 
map sqrt [2.0; 3.0] ;; 
map sin [2.0; 3.0] ;; 
 
(* 13.3 *) 
 
#use "sect13_3/length.ml" ;; 
let is_zero n = n = 0 ;; 
map is_zero [2; 1; 0; -1; -2] ;; 
 
(* 13.4 *) 
 
3 ;; 
sqrt ;; 
let twice7 f = f (f 7) ;; 
let add3 x = x + 3 ;; 
twice7 add3 ;; 
 
(* 13.5 *) 
 
let twice f = 
  let g x = f (f x) 
  in g ;; 
let times2 x = x * 2 ;; 
let add6 = twice add3 ;; 
add6 8 ;; 
add6 9 ;; 
 
(* 13.6 *) 
