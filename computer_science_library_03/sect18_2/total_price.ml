(* あらかじめ sect18_1/price.ml を読み込んでおく必要あり *) 
#use "sect18_1/price.ml" 
 
(* 八百屋においてある野菜と値段のリストの例 *) 
let yaoya_list = [("トマト", 300); ("たまねぎ", 200); 
                  ("にんじん", 150); ("ほうれん草", 200)] 
 
(* 目的：yasai_list を買ったときの値段の合計を調べる *) 
(* total_price : string list -> (string * int) list -> int *) 
let rec total_price yasai_list yaoya_list = match yasai_list with 
    [] -> 0 
  | first :: rest -> 
      match price first yaoya_list with 
          None -> total_price rest yaoya_list 
        | Some (p) -> p + total_price rest yaoya_list 
 
(* テスト *) 
let test1 = total_price ["トマト"; "にんじん"] yaoya_list = 450 
let test2 = total_price ["じゃがいも"; "たまねぎ"; "にんじん"] yaoya_list = 350 
