(* 目的：受け取った ekimei0 までの距離を lst から探して返す *) 
(* 見つからなかったら例外 Not_found を起こす *) 
(* assoc : 'a -> ('a * 'b) list -> 'b *) 
let rec assoc ekimei0 lst = match lst with 
    [] -> raise Not_found 
  | (ekimei, kyori) :: rest -> 
      if ekimei = ekimei0 then kyori else assoc ekimei0 rest 
 
(* 'b についても多相の型となる。*) 
 
(* テスト *) 
(* let test1 = assoc "後楽園" [] *) 
   (* Not_found を起こす *) 
let test2 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8 
(* let test3 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] *) 
   (* Not_found を起こす *) 
