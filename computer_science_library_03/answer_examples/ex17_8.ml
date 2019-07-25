(* あらかじめ sect17_2/tree.ml を読み込んでおく必要あり *) 
#use "sect17_2/tree.ml" (* tree_t の定義 *) 
 
(* 木の例 *) 
let tree1 = Empty 
let tree2 = Leaf (3) 
let tree3 = Node (tree1, 4, tree2) 
let tree4 = Node (tree2, 5, tree3) 
 
(* 目的：tree に含まれる節と葉の数の合計を返す *) 
(* tree_depth : tree_t -> int *) 
let rec tree_depth tree = match tree with 
    Empty -> 0 
  | Leaf (n) -> 0 
  | Node (t1, n, t2) -> 1 + max (tree_depth t1) (tree_depth t2) 
 
(* テスト *) 
let test1 = tree_depth tree1 = 0 
let test2 = tree_depth tree2 = 0 
let test3 = tree_depth tree3 = 1 
let test4 = tree_depth tree4 = 2 
