(* 目的：引数 n の階乗を返す *) 
(* Fac.f : int -> int *) 
let rec f n = 
  if n = 0 then 1 else n * f (n - 1) 
