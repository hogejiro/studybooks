(* あらかじめ ex09_9.ml, ex18_6.ml を読み込んでおく必要あり *) 
#use "ex09_9.ml" (* ekimei_t, global_ekimei_list の定義 *) 
#use "ex18_6.ml" (* No_such_station の定義 *) 
 
(* 目的：ローマ字の駅名を漢字に直す *) 
(* 見つからなかったら例外 No_such_station を起こす *) 
(* romaji_to_kanji : string -> ekimei_t list -> string *) 
let rec romaji_to_kanji r0 ekimei_list = match ekimei_list with 
    [] -> raise (No_such_station (r0)) 
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest -> 
      if r0 = r then k else romaji_to_kanji r0 rest 
 
(* テスト *) 
let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷" 
let test2 = romaji_to_kanji "shibuya" global_ekimei_list = "渋谷" 
let test3 = romaji_to_kanji "otemachi" global_ekimei_list = "大手町" 
(* let test4 = romaji_to_kanji "osaka" global_ekimei_list *) 
   (* No_such_station "osaka" を起こす *) 
