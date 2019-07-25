(* あらかじめ ex09_10.ml, ex17_11.ml, ex17_13.ml を読み込んでおく必要あり *) 
#use "ex09_10.ml" (* ekikan_t, global_ekikan_list の定義 *) 
#use "ex17_11.ml" (* assoc の定義 *) 
#use "ex17_13.ml" (* inserts_ekikan の定義 *) 
 
(* 目的：ふたつの駅の間の距離を求める *) 
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *) 
let rec get_ekikan_kyori eki1 eki2 tree = match tree with 
    Empty -> infinity 
  | Node (left, k, lst, right) -> 
      if eki1 < k then get_ekikan_kyori eki1 eki2 left 
      else if k < eki1 then get_ekikan_kyori eki1 eki2 right 
      else assoc eki2 lst 
 
(* テスト *) 
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list 
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 
let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6 
