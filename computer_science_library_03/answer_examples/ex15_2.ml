(* 目的：m > n > 0 なる自然数 m と n の最大公約数を求める *) 
(* gcd : int -> int -> int *) 
let rec gcd m n = 
  if n = 0 then m else gcd n (m mod n) 
 
(* 再帰のたびに n の値が小さくなっているので、いずれ 0 になり停止する *) 
 
(* テスト *) 
let test1 = gcd 7 5 = 1 
let test2 = gcd 30 18 = 6 
let test3 = gcd 36 24 = 12 
