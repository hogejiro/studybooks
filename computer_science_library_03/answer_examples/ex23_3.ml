(* このファイルは RedBlack モジュール、または Tree モジュールに加え、 
   Heap モジュール（sect23_5/heap.mli, sect23_7/heap.ml を参照）、 
   そして、駅名、駅間のリストをひとつのファイルにまとめた metro.ml 
   と一緒にコンパイルする *) 
 
open Metro (* global_ekimei_list, global_ekikan_list の定義 *) 
open RedBlack 
(* open Tree *) (* Tree モジュールを使う場合はこちらを使用する *) 
 
(* グラフの中の節（駅）を表す型 *) 
(* ただし、ここでは dijkstra_main の結果を格納するためにしか使っていない *) 
type eki_t = { 
  namae        : string;       (* 駅名（漢字） *) 
  saitan_kyori : float;        (* 最短距離 *) 
  temae_list   : string list;  (* 手前の駅名（漢字）のリスト *) 
} 
 
(* 駅間情報を表す木の型。以下のコメントでは ekikan_tree_t と記述 *) 
(* type ekikan_tree_t = (string, (string * float) list) Tree.t *) 
 
(* 目的：重複を取り除き、各点の接続情報を示す木 ekikan_tree を作る。*) 
(* 駅間情報のみから構築し、駅情報は使わない。*) 
(* make_ekikan_tree : ekikan_t list -> ekikan_tree_t *) 
let make_ekikan_tree ekikan_list = 
  let insert ekikan_tree kiten shuten kyori = 
    let lst = try 
		search ekikan_tree kiten 
	      with Not_found -> [] in 
    insert ekikan_tree kiten ((shuten, kyori) :: lst) in 
  let insert_ekikan ekikan_tree 
		    {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} = 
    insert (insert ekikan_tree k s r) s k r in 
  List.fold_left insert_ekikan empty ekikan_list 
 
(* 未確定駅の情報を表すヒープの型。以下のコメントでは heap_t と記述 *) 
(* type heap_t = (float,			最短距離 
		  string *			駅名 
		  string list *			手前リスト 
		 ) Heap.t *) 
 
(* 各駅のヒープ中の位置を表す木の型。以下のコメントでは index_tree_t と記述 *) 
(* type index_tree_t = (string, index_t) Tree.t *) 
 
(* 目的：ekikan_tree から eki_heap と index_tree を作り、kiten を初期化する *) 
(* make_eki_heap_and_index_tree : 
	string -> ekikan_tree_t -> heap_t * index_tree_t *) 
let make_eki_heap_and_index_tree kiten ekikan_tree = 
  traverse (fun (eki_heap, index_tree) k lst -> 
	      let (index, heap) = Heap.insert eki_heap 
					      (if k = kiten then 0. 
							    else infinity) 
					      (k, if k = kiten then [k] 
						  else []) in 
	      let index_tree' = insert index_tree k index in 
	      (heap, index_tree')) 
	   ((Heap.create (length ekikan_tree) 0. ("駅名", [])), 
	    empty) 
	   ekikan_tree 
 
(* 目的：確定した駅に接続している駅の最短距離、手前リストを更新する *) 
(* koushin : string -> float -> string list -> 
	     heap_t -> ekikan_tree_t -> index_tree_t -> heap_t *) 
let koushin pn ps pt eki_heap ekikan_tree index_tree = 
  let lst = search ekikan_tree pn in 
  List.fold_left (fun eki_heap (shuten, kyori) -> try 
		    let shuten_index = search index_tree shuten in 
		    let (saitan_kyori, (n, _)) = 
		      Heap.get eki_heap shuten_index in 
		    let new_saitan_kyori = ps +. kyori in 
		    if new_saitan_kyori <= saitan_kyori 
		    then Heap.set eki_heap shuten_index 
				  new_saitan_kyori (n, (n :: pt)) 
		    else eki_heap 
		  with Not_found -> eki_heap) 
		 eki_heap 
		 lst 
 
(* 目的：未確定駅のリストと駅間リストから、各駅への最短路を求める *) 
(* dijkstra_main : heap_t -> ekikan_tree_t -> index_tree_t -> eki_t list *) 
let rec dijkstra_main eki_heap ekikan_tree index_tree = 
  if Heap.length eki_heap = 0 
  then [] 
  else let ((ps, (pn, pt)), rest_heap) = Heap.split_top eki_heap in 
       let eki_heap2 = koushin pn ps pt rest_heap ekikan_tree index_tree in 
       {namae = pn; saitan_kyori = ps; temae_list = pt} 
       :: dijkstra_main eki_heap2 ekikan_tree index_tree 
 
(* 駅名が見つからなかったことを示す例外 *) 
exception No_such_station of string 
 
(* 目的：ローマ字の駅名を漢字に直す *) 
(* 見つからなかったら例外 No_such_station を起こす *) 
(* romaji_to_kanji : string -> ekimei_t list -> string *) 
let rec romaji_to_kanji r0 ekimei_list = match ekimei_list with 
    [] -> raise (No_such_station (r0)) 
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest -> 
      if r0 = r then k else romaji_to_kanji r0 rest 
 
(* 目的：受け取った eki_list から shuten のレコードを探し出す *) 
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 
 
(* 目的：始点と終点を受け取ったら、最短路を求め、終点のレコードを返す *) 
(* dijkstra : string -> string -> eki_t *) 
let dijkstra romaji_kiten romaji_shuten = 
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in 
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in 
  let ekikan_tree = make_ekikan_tree global_ekikan_list in 
  let (eki_heap, index_tree) = 
	make_eki_heap_and_index_tree kiten ekikan_tree in 
  let eki_list = dijkstra_main eki_heap ekikan_tree index_tree in 
  find shuten eki_list 
 
(* 目的：eki_t 型のレコードをきれいに表示する *) 
(* print_eki : eki_t -> unit *) 
let print_eki eki = match eki with 
  {namae = n; saitan_kyori = s; temae_list = lst} -> match lst with 
      [] -> assert false (* この場合は起こりえない *) 
    | [a] -> print_string (a ^ "（" ^ string_of_float s ^ "km）"); 
	     print_newline () 
    | a :: rest -> List.fold_right (fun b () -> print_string (b ^ "、")) 
				   rest (); 
		   print_string (a ^ "（" ^ string_of_float s ^ "km）"); 
		   print_newline () 
 
(* メイン関数 *) 
(* main : string -> string -> unit *) 
let main romaji_kiten romaji_shuten = 
  let eki = dijkstra romaji_kiten romaji_shuten in 
  print_eki eki 
 
(* メイン関数の呼び出し *) 
let _ = main Sys.argv.(1) Sys.argv.(2) 
 
(* 実行例 
 
% cat Makefile 
SOURCES = redBlack.mli redBlack.ml heap.mli heap.ml metro.ml ex23_3.ml 
RESULT = dijkstra 
OCAMLMAKEFILE = OCamlMakefile 
include $(OCAMLMAKEFILE) 
% make 
... 
% ./dijkstra shibuya gokokuji 
渋谷、表参道、青山一丁目、永田町、麹町、市ヶ谷、飯田橋、江戸川橋、護国寺、 
（9.8km） 
% ./dijkstra myogadani meguro 
茗荷谷、後楽園、飯田橋、市ヶ谷、麹町、永田町、溜池山王、六本木一丁目、 麻 
布十番、白金高輪、白金台、目黒（12.7km） 
% 
 
*) 
