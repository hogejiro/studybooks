(* あらかじめ ex12_1.ml を読み込んでおく必要あり *) 
#use "ex12_1.ml" (* eki_t の定義 *) 
 
(* 目的：受け取った駅のリストを、最短距離最小の駅とそれ以外に分離する *) 
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *) 
let rec saitan_wo_bunri eki eki_list = match eki_list with 
    [] -> (eki, []) 
  | first :: rest -> 
      let (p, v) = saitan_wo_bunri first rest in 
      match (eki, p) with 
	({namae = fn; saitan_kyori = fs; temae_list = ft}, 
	 {namae = sn; saitan_kyori = ss; temae_list = st}) -> 
	  if fs < ss then (eki, p :: v) 
	  else (p, eki :: v) 
 
(* または *) 
 
(* 目的：受け取った駅のリストを、最短距離最小の駅とそれ以外に分離する *) 
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *) 
let saitan_wo_bunri eki eki_list = 
  List.fold_right (fun first (p, v) -> 
		     match (first, p) with 
		       ({namae = fn; saitan_kyori = fs; temae_list = ft}, 
			{namae = sn; saitan_kyori = ss; temae_list = st}) -> 
			 if fs < ss then (first, p :: v) 
			 else (p, first :: v)) 
		  eki_list 
		  (eki, []) 
 
(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* テスト *) 
let test = saitan_wo_bunri eki1 [eki2; eki3; eki4] = (eki3, [eki2; eki1; eki4]) 
