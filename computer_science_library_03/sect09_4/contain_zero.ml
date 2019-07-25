(* int list は 
     - []              空リスト、あるいは 
     - first :: rest   最初の要素が first で残りのリストが rest 
   という形 *) 
 
(* 目的：受け取ったリスト lst に 0 が含まれているかを調べる *) 
(* contain_zero : int list -> bool *) 
let rec contain_zero lst = match lst with 
    [] -> false 
  | first :: rest -> if first = 0 then true 
                                  else contain_zero rest 
 
(* テスト *) 
let test1 = contain_zero [] = false 
let test2 = contain_zero [0; 2] = true 
let test3 = contain_zero [1; 2] = false 
let test4 = contain_zero [1; 2; 3; 0; 5; 6; 7] = true 
let test5 = contain_zero [1; 2; 3; 4; 5; 6; 7] = false 
