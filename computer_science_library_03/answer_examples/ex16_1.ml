(* 目的：先頭からリスト中の各整数までの合計を計算する *) 
(* sum_list : int list -> int list *) 
let sum_list lst = 
  (* 目的：先頭からリスト中の各整数までの合計を計算する *) 
  (* ここで total0 はこれまでの整数の合計 *) 
  (* hojo : int list -> int -> int list *) 
  let rec hojo lst total0 = match lst with 
      [] -> [] 
    | first :: rest -> first + total0 :: hojo rest (first + total0) 
  in hojo lst 0 
 
(* テスト *) 
let test1 = sum_list [] = [] 
let test2 = sum_list [1; 2; 3] = [1; 3; 6] 
let test3 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10] 
