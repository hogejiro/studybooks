(* あらかじめ ex05_4.ml を読み込んでおく必要あり *) 
#use "ex05_4.ml" 
 
(* 目的：a, b, c を受け取ったら解の個数を返す *) 
(* kai_no_kosuu : float -> float -> float -> int *) 
let kai_no_kosuu a b c = 
  if hanbetsushiki a b c > 0.0 then 2 
  else if hanbetsushiki a b c = 0.0 then 1 
  else 0 
 
(* テスト *) 
let test1 = kai_no_kosuu 1.0 5.0 4.0 = 2 
let test2 = kai_no_kosuu 2.0 (-4.0) 2.0 = 1 
let test3 = kai_no_kosuu 1.0 2.0 4.0 = 0 
