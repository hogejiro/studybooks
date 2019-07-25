(* あらかじめ ex09_10.ml, ex12_1.ml, ex17_13.ml を読み込んでおく必要あり *) 
#use "ex09_10.ml" (* ekikan_t, global_ekikan_list の定義 *) 
#use "ex12_1.ml" (* eki_t の定義 *) 
#use "ex17_13.ml" (* inserts_ekikan の定義 *) 
 
(* list 版 *) 
 
(* 目的：ふたつの駅の間の距離を求める *) 
(* 見つからなかったら例外 Not_found を起こす *) 
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *) 
let rec get_ekikan_kyori eki1 eki2 lst = match lst with 
    [] -> raise Not_found 
  | {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} :: rest -> 
     if (eki1 = k && eki2 = s) || (eki1 = s && eki2 = k) 
     then r 
     else get_ekikan_kyori eki1 eki2 rest 
 
(* テスト *) 
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2 
(* let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_list *) 
   (* Not_found を起こす *) 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_list = 0.6 
 
(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *) 
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *) 
let koushin p v ekikan_list = match p with 
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
	       {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
		 try 
		   let kyori = get_ekikan_kyori pn qn ekikan_list in 
		   if ps +. kyori < qs 
		   then {namae = qn; saitan_kyori = ps +. kyori; 
				     temae_list = qn :: pt} 
		   else q 
		 with Not_found -> q) 
	     v 
 
(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let test1 = koushin eki2 [] global_ekikan_list = [] 
let test2 = koushin eki2 lst global_ekikan_list = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
 
(* tree 版 *) 
 
(* 目的：ふたつの駅の間の距離を求める *) 
(* 見つからなかったら例外 Not_found を起こす *) 
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *) 
let rec get_ekikan_kyori eki1 eki2 tree = match tree with 
    Empty -> raise Not_found 
  | Node (left, k, lst, right) -> 
      if eki1 < k then get_ekikan_kyori eki1 eki2 left 
      else if k < eki1 then get_ekikan_kyori eki1 eki2 right 
      else List.assoc eki2 lst 
 
(* テスト *) 
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list 
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 
(* let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree *) 
   (* Not_found を起こす *) 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6 
 
(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *) 
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *) 
let koushin p v ekikan_tree = match p with 
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
	       {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
		 try 
		   let kyori = get_ekikan_kyori pn qn ekikan_tree in 
		   if ps +. kyori < qs 
		   then {namae = qn; saitan_kyori = ps +. kyori; 
				     temae_list = qn :: pt} 
		   else q 
		 with Not_found -> q) 
	     v 
 
(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let test1 = koushin eki2 [] global_ekikan_tree = [] 
let test2 = koushin eki2 lst global_ekikan_tree = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
