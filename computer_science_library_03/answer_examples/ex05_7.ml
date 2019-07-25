(* あらかじめ ex04_4.ml を読み込んでおく必要あり *) 
#use "ex04_4.ml" 
 
(* 目的：身長と体重を受け取ったら体型を返す *) 
(* taikei : float -> float -> string *) 
let taikei shincho taiju = 
  if bmi shincho taiju < 18.5 then "やせ" 
  else if bmi shincho taiju < 25.0 then "標準" 
  else if bmi shincho taiju < 30.0 then "肥満" 
  else "高度肥満" 
 
(* テスト *) 
let test1 = taikei 1.13 20.0 = "やせ" 
let test2 = taikei 1.63 57.0 = "標準" 
let test3 = taikei 1.71 56.0 = "標準" 
