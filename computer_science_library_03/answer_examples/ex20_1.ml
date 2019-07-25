(* あらかじめ sect20_1/color.ml を読み込んでおく必要あり *) 
#use "sect20_1/color.ml" 
 
(* 木を表す型 *) 
type ('a, 'b) rb_tree_t = 
    Empty 
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t 
