(* 年号を表す型 *) 
type nengou_t = Meiji of int    (* 明治 *) 
              | Taisho of int   (* 大正 *) 
              | Showa of int    (* 昭和 *) 
              | Heisei of int   (* 平成 *) 
 
(* 目的：年号を受け取ったら対応する西暦年を返す *) 
(* to_seireki : nengou_t -> int *) 
let to_seireki nengou = match nengou with 
    Meiji (n) -> n + 1867 
  | Taisho (n) -> n + 1911 
  | Showa (n) -> n + 1925 
  | Heisei (n) -> n + 1988 
 
(* テスト *) 
let test1 = to_seireki (Showa (42)) = 1967 
let test2 = to_seireki (Heisei (11)) = 1999 
let test3 = to_seireki (Meiji (41)) = 1908 
