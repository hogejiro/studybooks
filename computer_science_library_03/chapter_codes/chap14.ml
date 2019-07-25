(* 14.1 *) 
 
#use "sect14_1/filter.ml" ;; 
 
(* 14.2 *) 
 
#use "sect14_2/fold_right.ml" ;; 
 
(* 14.3 *) 
 
#use "sect14_3/sum.ml" ;; 
 
(* 14.4 *) 
 
fun x -> x + 1 ;; 
(fun x -> x + 1) 5 ;; 
let add1 = fun x -> x + 1 ;; 
let add1 x = x + 1 ;; 
#use "sect14_4/sum.ml" ;; 
 
(* 14.5 *) 
 
(+) 3 5 ;; 
#use "sect14_5/sum.ml" ;; 
 
(* 14.6 *) 
 
#use "sect14_6/perfect.ml" ;; 
enumerate 5 ;; 
divisor 24 ;; 
perfect 10000 ;; 
