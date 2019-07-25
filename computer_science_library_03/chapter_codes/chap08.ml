(* 8.1 *) 
 
("asai", 70) ;; 
 
(* 8.5 *) (* ここで宣言しないと 8.2 以下が実行できない *) 
 
type gakusei_t = { 
  namae : string; 
  tensuu : int; 
  seiseki : string; 
} ;; 
 
(* 8.2 *) 
 
{namae = "asai"; tensuu = 70; seiseki = "B"} ;; 
{tensuu = 70; namae = "asai"; seiseki = "B"} ;; 
(* 
{namae = "asai"} ;; 
*) 
 
(* 8.3 *) 
 
let tsuuchi gakusei = match gakusei with 
  {namae = n; tensuu = t; seiseki = s} -> 
    n ^ " さんは " ^ string_of_int t ^ " 点で、成績は " ^ 
    s ^ " です。" ;; 
tsuuchi {namae="asai"; tensuu=70; seiseki="B"} ;; 
let hyouka gakusei = match gakusei with 
  {namae = n; tensuu = t; seiseki = s} -> 
    {namae = n; 
     tensuu = t; 
     seiseki = if t >= 80 then "A" 
               else if t >= 70 then "B" 
               else if t >= 60 then "C" else "D"} ;; 
hyouka {namae="asai"; tensuu=70; seiseki=""} ;; 
tsuuchi (hyouka {namae="asai"; tensuu=70; seiseki=""}) ;; 
 
(* 8.4 *) 
let asai = {namae = "asai"; tensuu = 70; seiseki = "B"} ;; 
asai.namae ;; 
asai.tensuu ;; 
asai.seiseki ;; 
 
(* 8.5 *) (* 8.1 の次を参照 *) 
 
(* 8.6 *) 
 
#use "sect08_6/hyouka.ml" ;; 
 
(* 8.7 *) 
