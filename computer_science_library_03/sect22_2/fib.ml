(* 目的：フィボナッチ数を再帰回数とともに求める *) 
(* fib : int -> int -> (int * int) *) 
let rec fib n c =                  (* c はこれまでに呼ばれた回数 *) 
  let c0 = c + 1 in                (* カウンタに 1 を加える      *) 
  if n < 2 
  then (n, c0)                     (* カウンタを一緒に返す       *) 
  else let (r1, c1) = fib (n - 1) c0 in 
       (* c0 からはじめて fib (n - 1) 中での呼び出し回数を数える *) 
       let (r2, c2) = fib (n - 2) c1 in 
       (* c1 からはじめて fib (n - 2) 中での呼び出し回数を数える *) 
       (r1 + r2, c2)               (* c2 が全体の呼び出し回数    *) 
 
(* テスト *) 
let test = fib 8 0 = (21, 67) 
