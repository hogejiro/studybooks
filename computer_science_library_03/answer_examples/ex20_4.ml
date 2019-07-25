(* あらかじめ ex20_1.ml を読み込んでおく必要あり *) 
#use "ex20_1.ml" (* rb_tree_t の定義 *) 
 
(* 目的：赤黒木とキーを受け取ったら、そのキーに対応する値を返す *) 
(* search : ('a, 'b) rb_tree_t -> 'a -> 'b *) 
let rec search rb_tree k = match rb_tree with 
    Empty -> raise Not_found 
  | Node (left, key, value, color, right) -> 
      if k = key then value 
      else if k < key then search left k 
      else search right k 
 
(* テスト *) 
let rb_tree = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
	Node (Empty, 15, "z", Black, Empty)) 
let test1 = search rb_tree 10 = "x" 
let test2 = search rb_tree 13 = "y" 
let test3 = search rb_tree 15 = "z" 
(* let test4 = search rb_tree 17 *) 
   (* Not_found を起こす *) 
