(* 赤組か白組かを表す型 *) 
type team_t = Red | White 
 
(* 目的：受け取ったチーム名を文字列で返す *) 
(* team_string : team_t -> string *) 
let team_string team = match team with 
    Red -> "赤組" 
  | White -> "白組" 
 
(* テスト *) 
let test1 = team_string Red = "赤組" 
let test2 = team_string White = "白組" 
