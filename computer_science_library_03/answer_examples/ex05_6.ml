(* あらかじめ ex05_4.ml を読み込んでおく必要あり *) 
#use "ex05_4.ml" 
 
(* 目的：a, b, c を受け取ったら虚数解を持つかを判定する *) 
(* kyosuukai : float -> float -> float -> bool *) 
let kyosuukai a b c = 
  hanbetsushiki a b c < 0.0 
 
(* テスト *) 
let test1 = kyosuukai 1.0 5.0 4.0 = false 
let test2 = kyosuukai 2.0 (-4.0) 2.0 = false 
let test3 = kyosuukai 1.0 2.0 4.0 = true 
