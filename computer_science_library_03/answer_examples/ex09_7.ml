(* あらかじめ ex08_3.ml を読み込んでおく必要あり *) 
#use "ex08_3.ml" 
 
(* person_t list 型のデータの例 *) 
let lst1 = [] 
let lst2 = [person1] 
let lst3 = [person2] 
let lst4 = [person1; person2; person3] 
let lst5 = [person2; person1; person1] 
 
(* person1, person2, person3 の定義については ex08_3.ml を参照 *) 
 
(* 目的：血液型が A の人の数を返す *) 
(* count_ketsueki_A : person_t list -> int *) 
let rec count_ketsueki_A lst = match lst with 
    [] -> 0 
  | {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; 
     ketsueki = k} :: rest -> 
      if k = "A" then 1 + count_ketsueki_A rest 
                 else count_ketsueki_A rest 
 
(* テスト *) 
let test1 = count_ketsueki_A lst1 = 0 
let test2 = count_ketsueki_A lst2 = 1 
let test3 = count_ketsueki_A lst3 = 0 
let test4 = count_ketsueki_A lst4 = 1 
let test5 = count_ketsueki_A lst5 = 2 
