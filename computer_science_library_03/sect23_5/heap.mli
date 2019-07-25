(* ヒープを表すモジュールのシグネチャ *) 
 
type ('a, 'b) t 
  (* 最小値を求める値が 'a 型で 
     そのほかの付加情報が 'b 型であるヒープの型 *) 
 
val create : int -> 'a -> 'b -> ('a, 'b) t 
  (* 使い方：create size key value *) 
  (* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら *) 
  (* 空のヒープを返す *) 
 
type index_t 
  (* ヒープの添字の型 *) 
 
val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t 
  (* 使い方：insert heap key value *) 
  (* ヒープに新しい要素を追加する *) 
  (* ヒープは（破壊的に）書き変わる *) 
 
val get : ('a, 'b) t -> index_t -> 'a * 'b 
  (* 使い方：get heap index *) 
  (* ヒープの index 番目の要素を返す *) 
 
val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t 
  (* 使い方：set heap index key value *) 
  (* ヒープの index 番目の値を更新したヒープを返す *) 
  (* ヒープは（破壊的に）書き変わる *) 
 
val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t 
  (* 使い方：split_top heap *) 
  (* 最小の値を持つものとそれを取り除いたヒープのペアを返す *) 
  (* ヒープは（破壊的に）書き変わる *) 
 
val length : ('a, 'b) t -> int 
  (* 使い方：length heap *) 
  (* ヒープ中のデータの数を返す *) 
