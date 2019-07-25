(* メイン関数 *) 
(* main : int -> unit *) 
let main n = 
  let kekka = Fac.f n in        (* n の階乗を求め *) 
  (print_int n; 
   print_string " の階乗は "; 
   print_int kekka;             (* 結果を表示し   *) 
   print_string " です。"; 
   print_newline ())            (* 改行する       *) 
 
(* メイン関数の呼び出し *) 
let _ = main (int_of_string Sys.argv.(1)) 
