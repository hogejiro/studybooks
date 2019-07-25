(* あらかじめ sect17_1/to_seireki.ml を読み込んでおく必要あり *) 
#use "sect17_1/to_seireki.ml" 
 
(* 目的：生まれた年の年号と現在の年号を受け取ったら年齢を返す *) 
(* nenrei : nengou_t -> nengou_t -> int *) 
let nenrei nengou1 nengou2 = 
  to_seireki nengou2 - to_seireki nengou1 
 
(* テスト *) 
let test1 = nenrei (Showa (42)) (Heisei (18)) = 39 
let test2 = nenrei (Heisei (11)) (Heisei (18)) = 7 
let test3 = nenrei (Meiji (41)) (Heisei (17)) = 97 
