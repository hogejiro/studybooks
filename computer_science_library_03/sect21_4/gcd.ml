(* 目的：ふたつの自然数 m, n の最大公約数を求める *) 
(* gcd : int -> int -> int *) 
let rec gcd m n = 
 (print_string "m = "; 
  print_int m;           (* m の値を表示 *) 
  print_string ", n = "; 
  print_int n;           (* n の値を表示 *) 
  print_newline ();      (* 改行         *) 
  if n = 0 then m 
           else gcd n (m mod n)) 
 
(* テスト *) 
let test1 = gcd 7 5 = 1 
let test2 = gcd 30 18 = 6 
let test3 = gcd 36 24 = 12 
let test4 = gcd 216 126 = 18 
