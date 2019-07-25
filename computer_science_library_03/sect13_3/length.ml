(* 目的：受け取ったリスト lst の長さを求める *) 
(* length : 'a list -> int *) 
let rec length lst = match lst with 
    [] -> 0 
  | first :: rest -> 1 + length rest 
 
(* テスト *) 
let test1 = length [] = 0 
let test2 = length [3] = 1 
let test3 = length [2; 1; 6; 4; 7] = 5 
