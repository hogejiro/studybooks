(* あらかじめ ex08_3.ml を読み込んでおく必要あり *) 
#use "ex08_3.ml" 
 
(* person_t list 型のデータの例 *) 
let lst = [person1; person2; person3] 
 
(* person1, person2, person3 の定義については ex08_3.ml を参照 *) 
 
(* 目的：person_t 型のデータから名前を返す *) 
(* take_name : person_t -> string *) 
let take_name person = match person with 
  {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k} -> n 
 
(* テスト *) 
let test1 = take_name person1 = "浅井" 
let test2 = take_name person2 = "宮原" 
let test3 = take_name person3 = "中村" 
 
(* 目的：lst に含まれる人の名前のリストを返す *) 
(* person_name : person_t list -> string list *) 
let person_name lst = List.map take_name lst 
 
(* テスト *) 
let test4 = person_name [] = [] 
let test5 = person_name lst = ["浅井"; "宮原"; "中村"] 
