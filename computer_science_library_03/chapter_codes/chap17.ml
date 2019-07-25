(* 17.1 *) 
 
type team_t = Red | White ;; 
Red ;; 
White ;; 
(* 
let A = 3 ;; 
*) 
#use "sect17_1/team_string.ml" ;; 
let team_string team = match team with 
  Red -> "赤組" ;; 
 
#use "sect17_1/to_seireki.ml" ;; 
Showa (42) ;; 
(* 
Meiji (true) ;; 
Taisho ;; 
*) 
 
(* 17.2 *) 
 
#use "sect17_2/tree.ml" ;; 
Empty ;; 
Leaf (3) ;; 
Leaf (24) ;; 
Node (Empty, 7, Leaf (3)) ;; 
Node (Node (Empty, 7, Leaf (3)), 17, Leaf (24)) ;; 
 
(* 17.3 *) 
 
#use "sect17_3/sum_tree.ml" ;; 
 
(* 17.4 *) 
 
#use "sect17_4/search.ml" ;; 
 
(* 17.5 *) 
 
search ;; 
insert_tree ;; 
#use "sect17_5/search.ml" ;; 
search ;; 
insert_tree ;; 
 
(* 17.6 *) 
 
(* 17.7 *) 
 
(* 17.8 *) 
