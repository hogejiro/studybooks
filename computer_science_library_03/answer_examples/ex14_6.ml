(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *) 
type gakusei_t = { 
  namae : string;       (* 名前 *) 
  tensuu : int;         (* 点数 *) 
  seiseki : string;     (* 成績 *) 
} 
 
(* gakusei_t list 型のデータの例 *) 
let lst1 = [] 
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}] 
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"}; 
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"}; 
            {namae = "asai"; tensuu = 70; seiseki = "B"}; 
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}] 
 
(* 目的：学生リスト lst のうち成績が seiseki0 の人の数を返す *) 
(* count : gakusei_t list -> string -> int *) 
let count lst seiseki0 = 
  (* 目的：学生の成績が A かどうかを調べる *) 
  (* is_seiseki0 : gakusei_t -> bool *) 
  let is_seiseki0 gakusei = match gakusei with 
    {namae = n; tensuu = t; seiseki = s} -> s = seiseki0 
  in List.length (List.filter is_seiseki0 lst) 
 
(* 目的：学生リスト lst のうち成績が A の人の数を返す *) 
(* count_A : gakusei_t list -> int *) 
let count_A lst = count lst "A" 
 
(* テスト *) 
let test1 = count_A lst1 = 0 
let test2 = count_A lst2 = 0 
let test3 = count_A lst3 = 1 
let test4 = count_A lst4 = 2 
let test5 = count lst4 "B" = 1 
let test6 = count lst4 "C" = 0 
