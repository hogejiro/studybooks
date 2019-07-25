(* あらかじめ ex08_3.ml を読み込んでおく必要あり *) 
#use "ex08_3.ml" 
 
(* 目的：person_t 型のデータから血液型を示す文を返す *) 
(* ketsueki_hyoji : person_t -> string *) 
let ketsueki_hyoji person = match person with 
  {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k} -> 
    n ^ "さんの血液型は " ^ k ^ " 型です" 
 
(* テスト *) 
let test1 = ketsueki_hyoji person1 = "浅井さんの血液型は A 型です" 
let test2 = ketsueki_hyoji person2 = "宮原さんの血液型は B 型です" 
let test3 = ketsueki_hyoji person3 = "中村さんの血液型は O 型です" 
 
(* person1, person2, person3 の定義については ex08_3.ml を参照 *) 
