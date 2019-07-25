(* 目的：受け取ったふたつの関数を合成した関数を返す *) 
(* compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *) 
let compose f g = 
  let h x = f (g x) in 
  h 
 
(* 目的：3 を加える *) 
(* add3: int -> int *) 
let add3 x = x + 3 
 
(* 目的：2 を掛ける *) 
(* times2: int -> int *) 
let times2 x = x * 2 
 
(* テスト *) 
let test = (compose times2 add3) 4 = 14 
