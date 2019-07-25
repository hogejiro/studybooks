(* 距離と距離の合計を持っている型 *) 
type distance_t = { 
  kyori : float;   (* 距離 *) 
  total : float;   (* 距離の合計 *) 
} 
 
(* 目的：先頭からリスト中の各点までの距離の合計を計算する *) 
(* total_distance : distance_t list -> distance_t list *) 
let total_distance lst = 
  (* 目的：先頭からリスト中の各点までの距離の合計を計算する *) 
  (* ここで total0 はこれまでの距離の合計 *) 
  (* hojo : distance_t list -> float -> distance_t list *) 
  let rec hojo lst total0 = match lst with 
      [] -> [] 
    | {kyori = k; total = t} :: rest -> 
        {kyori = k; total = total0 +. k} 
        :: hojo rest (total0 +. k) 
  in hojo lst 0.0 
 
(* テスト *) 
let test = total_distance [{kyori = 0.3; total = 0.}; 
			   {kyori = 0.9; total = 0.}; 
			   {kyori = 1.4; total = 0.}; 
			   {kyori = 0.8; total = 0.}] 
	   = [{kyori = 0.3; total = 0.3}; 
	      {kyori = 0.9; total = 1.2}; 
	      {kyori = 1.4; total = 2.6}; 
	      {kyori = 0.8; total = 3.4}] 
 
(* ただし、このテストは float の誤差により true にはならない *) 
