(* 目的：名前と成績の組を受け取ったら、その評価文を返す *) 
(* seiseki : string * string -> string *) 
let seiseki pair = match pair with 
  (namae, seiseki0) -> namae ^ "さんの成績は " ^ seiseki0 ^ " です。" 
 
(* テスト *) 
let test1 = seiseki ("浅井", "C") = "浅井さんの成績は C です。" 
let test2 = seiseki ("中村", "A") = "中村さんの成績は A です。" 
let test3 = seiseki ("出原", "B") = "出原さんの成績は B です。" 
