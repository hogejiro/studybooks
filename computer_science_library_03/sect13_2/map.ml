(* あらかじめ sect08_6/hyouka.ml を読み込んでおく必要あり *) 
#use "sect08_6/hyouka.ml" 
 
(* 
(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *) 
(* map_sqrt : float list -> float list *) 
let rec map_sqrt lst = match lst with 
    [] -> [] 
  | first :: rest -> sqrt first :: map_sqrt rest 
*) 
 
(* 
(* 目的：学生のリスト lst を受け取り成績を入れたリストを返す *) 
(* map_hyouka : gakusei_t list -> gakusei_t list *) 
let rec map_hyouka lst = match lst with 
    [] -> [] 
  | first :: rest -> hyouka first :: map_hyouka rest 
*) 
 
(* 目的：関数 f とリスト lst を受け取り f を施したリストを返す *) 
(* map : ('a -> 'b) -> 'a list -> 'b list *) 
let rec map f lst = match lst with 
    [] -> [] 
  | first :: rest -> f first :: map f rest 
 
(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *) 
(* map_sqrt : float list -> float list *) 
let map_sqrt lst = map sqrt lst 
 
(* テスト *) 
let test1 = map_sqrt [] = [] 
let test2 = map_sqrt [1.0; 4.0] = [1.0; 2.0] 
let test3 = map_sqrt [9.0; 16.0; 4.0; 0.0] = [3.0; 4.0; 2.0; 0.0] 
 
(* 目的：学生のリスト lst を受け取り成績を入れたリストを返す *) 
(* map_hyouka : gakusei_t list -> gakusei_t list *) 
let map_hyouka lst = map hyouka lst 
 
(* gakusei_t list 型のデータの例 *) 
let lst1 = [] 
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}] 
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"}; 
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"}; 
            {namae = "asai"; tensuu = 70; seiseki = "B"}; 
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
 
(* テスト *) 
let test1 = map_hyouka lst1 = [] 
let test2 = map_hyouka lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}] 
let test3 = map_hyouka lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"}; 
			       {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
let test3 = map_hyouka lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"}; 
			       {namae = "asai"; tensuu = 70; seiseki = "B"}; 
			       {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
