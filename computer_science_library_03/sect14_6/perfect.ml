(* あらかじめ sect14_1/filter.ml, sect14_2/fold_right.ml を 
   読み込んでおく必要あり *) 
#use "sect14_1/filter.ml" 
#use "sect14_2/fold_right.ml" 
 
(* 目的：n から 1 までのリストを作る *) 
(* enumerate : int -> int list *) 
let rec enumerate n = 
  if n = 0 then [] else n :: enumerate (n - 1) 
 
(* 目的：n の約数のリストを返す *) 
(* divisor : int -> int list *) 
let divisor n = filter (fun x -> n mod x = 0) (enumerate n) 
 
(* 目的：m 以下の完全数のリストを返す *) 
(* perfect : int -> int list *) 
let perfect m = 
  filter (fun n -> fold_right (+) (divisor n) 0 - n = n) 
         (enumerate m) 
 
(* テスト *) 
let test1 = divisor 24 = [24; 12; 8; 6; 4; 3; 2; 1] 
let test2 = perfect 10000 = [8128; 496; 28; 6] 
