(* このファイルは 19.6 章で説明している my-tree.top の上で実行する *) 
(* my-tree.top は sect19_6 の下で OCamlMakefile をダウンロードした上で 
   make top を実行すると作成される *) 
 
(* あらかじめ ex09_10.ml, ex12_1.ml を読み込んでおく必要あり *) 
#use "ex09_10.ml" (* ekikan_t, global_ekikan_list の定義 *) 
#use "ex12_1.ml" (* eki_t の定義 *) 
 
(* 目的：受け取った kiten, shuten, kyori を ekikan_tree に挿入した木を返す *) 
(* insert1 : (string * (string * float) list) Tree.t -> 
	      string -> string -> float -> 
	     (string * (string * float) list) Tree.t *) 
let rec insert1 ekikan_tree kiten shuten kyori = 
  let lst = try 
	      Tree.search ekikan_tree kiten 
	    with Not_found -> [] 
  in Tree.insert ekikan_tree kiten ((shuten, kyori) :: lst) 
 
(* 目的：受け取った ekikan 情報を ekikan_tree に挿入した木を返す *) 
(* insert_ekikan : (string * (string * float) list) Tree.t -> 
		    ekikan_t -> 
		   (string * (string * float) list) Tree.t *) 
let insert_ekikan ekikan_tree ekikan = match ekikan with 
  {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} -> 
    insert1 (insert1 ekikan_tree s k r) k s r 
 
(* 目的：受け取った ekikan のリストを ekikan_tree に挿入した木を返す *) 
(* inserts_ekikan : (string * (string * float) list) Tree.t -> 
		     ekikan_t list -> 
		    (string * (string * float) list) Tree.t *) 
let inserts_ekikan ekikan_tree ekikan_list = 
  List.fold_left insert_ekikan ekikan_tree ekikan_list 
 
(* 目的：ふたつの駅の間の距離を求める *) 
(* 見つからなかったら例外 Not_found を起こす *) 
(* get_ekikan_kyori : string -> string -> 
		     (string * (string * float) list) Tree.t -> float *) 
let rec get_ekikan_kyori eki1 eki2 tree = 
  List.assoc eki2 (Tree.search tree eki1) 
 
(* テスト *) 
let global_ekikan_tree = inserts_ekikan Tree.empty global_ekikan_list 
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 
(* let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree *) 
   (* Not_found を起こす *) 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6 
