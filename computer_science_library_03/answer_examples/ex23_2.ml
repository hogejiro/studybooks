(* あらかじめ ex23_1.ml を読み込んでおく必要あり *) 
#use "ex23_1.ml" (* Heap モジュールの定義 *) 
 
(* 目的：受け取ったヒープの要素を小さい順に取り出してリストにして返す *) 
(* ここで lst はこれまでにヒープから取り出した要素のリスト *) 
(* extract_all_elements : ('a, unit) Heap.t -> 'a list -> 'a list *) 
let rec extract_all_elements heap lst = 
  try 
    let ((a, ()), heap) = Heap.split_top heap 
    in extract_all_elements heap (a :: lst) 
  with Heap.Empty -> lst 
 
(* 目的：受け取ったリストをヒープソートを使って大きい順に並べる *) 
(* heap_sort : 'a list -> 'a list *) 
let heap_sort lst = match lst with 
    [] -> [] 
  | a :: rest -> 
      let size = List.length lst in 
      let heap = List.fold_left 
		   (fun heap x -> 
		     let (_, heap) = Heap.insert heap x () in heap) 
		   (Heap.create size a ()) lst in 
      extract_all_elements heap [] 
 
(* テスト *) 
let test1 = heap_sort [] = [] 
let test2 = heap_sort [1] = [1] 
let test3 = heap_sort [1; 2] = [2; 1] 
let test4 = heap_sort [2; 1] = [2; 1] 
let test5 = heap_sort [5; 3; 8; 1; 7; 4] = [8; 7; 5; 4; 3; 1] 
