(* 1 *) 
 
(* f1 : 'a -> 'a *) 
let f1 x = x 
 
(* 2 *) 
 
(* f2 : 'a -> 'b -> 'a *) 
let f2 x y = x 
 
(* 3 *) 
 
(* f3 : 'a -> 'b -> 'b *) 
let f3 x y = y 
 
(* 4 *) 
 
(* f4 : 'a -> ('a -> 'b) -> 'b *) 
let f4 x f = f x 
 
(* 5 *) 
 
(* f5 : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *) 
let f5 f g x = g (f x) 
