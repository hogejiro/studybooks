(* あらかじめ sect14_2/fold_right.ml を読み込んでおく必要あり *) 
#use "sect14_2/fold_right.ml" 
 
(* 
(* 目的：受け取ったリスト lst の各要素の和を求める *) 
(* sum : int list -> int *) 
let sum lst = 
  fold_right (fun first rest_result -> first + rest_result) 
             lst 0 
*) 
 
(* 目的：受け取ったリスト lst の各要素の和を求める *) 
(* sum : int list -> int *) 
let sum lst = fold_right (+) lst 0 
 
(* テスト *) 
let test1 = sum [] = 0 
let test2 = sum [1; 2; 3] = 6 
let test3 = sum [1; 3; 7; 4; 2; 8] = 25 
