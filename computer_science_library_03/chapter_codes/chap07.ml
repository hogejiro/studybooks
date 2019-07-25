(* 7.1 *) 
 
(3.14, 2.71) ;; 
(3, true) ;; 
(3, "a", 3.14) ;; 
((3, "a"), 3.14) ;; 
3.14, 2.71 ;; 
 
(* 7.2 *) 
 
match (3, 5) with 
  (a, b) -> a + b ;; 
let add pair = match pair with 
  (a, b) -> a + b ;; 
add (3, 5) ;; 
let add (a, b) = a + b ;; 
(* 
let f pair = match pair with 
  (a, a) -> a + a ;; 
*) 
let add2 a b = a + b ;; 
add2 3 5 ;; 
 
(* 7.3 *) 
 
#use "sect07_3/add.ml" ;; 
 
(* 7.4 *) 
