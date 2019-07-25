(* 
(* メイン関数 *) 
(* main : unit -> unit *) 
let main () = 
  let kekka = Fac.f 10 in (* 10 の階乗を求め *) 
  print_int kekka         (* 結果を表示する  *) 
*) 
 
(* メイン関数 *) 
(* main : unit -> unit *) 
let main () = 
  let kekka = Fac.f 10 in       (* 10 の階乗を求め *) 
  (print_string "10 の階乗は "; 
   print_int kekka;             (* 結果を表示し    *) 
   print_string " です。"; 
   print_newline ())            (* 改行する        *) 
 
(* メイン関数の呼び出し *) 
let _ = main () 
