(* 目的：級数の第 n 項の値を求める *) 
(* dai_n_kou : int -> float *) 
let rec dai_n_kou n = 
  if n = 0 then 1.0 
           else dai_n_kou (n - 1) /. float_of_int n 
 
(* 
(* 目的：e の近似値を求める *) 
(* e : int -> float *) 
let rec e n = 
  if dai_n_kou n < 0.00001 
  then dai_n_kou n 
  else dai_n_kou n +. e (n + 1) 
*) 
 
(* 目的：e の近似値を求める *) 
(* e : int -> float *) 
let rec e n = 
  let d = dai_n_kou n in 
  if d < 0.00001 then d 
                 else d +. e (n + 1) 
 
(* テスト *) 
let test = e 0 
