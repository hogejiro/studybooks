(* あらかじめ sect14_2/fold_right.ml を読み込んでおく必要あり *) 
#use "sect14_2/fold_right.ml" 
 
(* 目的：受け取ったリスト lst の各要素の和を求める *) 
(* sum : int list -> int *) 
let sum lst = 
  fold_right (fun first rest_result -> first + rest_result) 
             lst 0 
 
(* テスト *) 
let test1 = sum [] = 0 
let test2 = sum [1; 2; 3] = 6 
let test3 = sum [1; 3; 7; 4; 2; 8] = 25 
 
(* 目的：受け取ったリスト lst の長さを求める *) 
(* length : 'a list -> int *) 
let length lst = 
  fold_right (fun first rest_result -> 1 + rest_result) lst 0 
 
(* テスト *) 
let test1 = length [] = 0 
let test2 = length [1; 2; 3] = 3 
let test3 = length [1; 3; 7; 4; 2; 8] = 6 
 
(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *) 
(* append : 'a list -> 'a list -> 'a list *) 
let append lst1 lst2 = 
  fold_right (fun first rest_result -> first :: rest_result) 
             lst1 lst2 
 
(* テスト *) 
let test1 = append [] [] = [] 
let test2 = append [1; 2; 3] [4; 5; 6] = [1; 2; 3; 4; 5; 6] 
let test3 = append [1; 3; 7; 4; 2; 8] [9; 6; 5] = [1; 3; 7; 4; 2; 8; 9; 6; 5] 
