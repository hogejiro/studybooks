(* 19.1 *) 
 
#use "sect19_1/zahyou.ml" ;; 
module Zahyou = struct 
  let x = 3.0 
  let y = 4.0 
end ;; 
Zahyou.x ;; 
 
(* 19.2 *) 
 
#use "sect19_2/tree.ml" ;; 
Tree.insert Tree.empty "a" 3 ;; 
 
(* 19.3 *) 
 
#use "sect19_3/tree.ml" ;; 
module NewTree = (Tree : Tree_t) ;; 
NewTree.insert NewTree.empty "a" 3 ;; 
 
(* 19.4 *) 
 
(* 19.5 *) 
 
#use "sect19_5/tree.ml" ;; 
 
(* 19.6 *) 
 
(* 19.7 *) 
