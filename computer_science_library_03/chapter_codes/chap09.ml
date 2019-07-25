(* 9.1 *) 
 
(* 9.2 *) 
 
1 :: 2 :: 3 :: [] ;; 
[1; 2; 3] ;; 
1 :: [2; 3] ;; 
1 :: 2 :: [3] ;; 
[true; false; true] ;; 
[] ;; 
let lst = [1; 2; 3; 4; 5] ;; 
 
(* 9.3 *) 
 
match [] with 
    [] -> 0 
  | first :: rest -> first ;; 
match [1; 2; 3] with 
    [] -> 0 
  | first :: rest -> first ;; 
match [1; 2; 3] with 
  first :: rest -> first ;; 
(* 
match [] with 
  first :: rest -> first ;; 
*) 
match [] with 
  [] -> 0 ;; 
 
(* 9.4 *) 
 
#use "sect09_4/contain_zero.ml" 
 
(* 9.5 *) 
 
#use "sect09_5/sum.ml" 
 
(* 9.6 *) 
 
#use "sect09_6/count_A.ml" 
 
(* 9.7 *) 
