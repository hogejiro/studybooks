(* 集合を表すシグネチャ *) 
module type Set_t = sig 
  type 'a t 
    (* 要素の型が 'a の集合の型。型の中身は非公開 *) 
 
  val empty : 'a t 
    (* 使い方：empty *) 
    (* 空集合 *) 
 
  val singleton : 'a -> 'a t 
    (* 使い方：singleton element *) 
    (* 要素が element ひとつからなる集合を返す *) 
 
  val union : 'a t -> 'a t -> 'a t 
    (* 使い方：union set1 set2 *) 
    (* 集合 set1 と set2 の和集合を返す *) 
 
  val inter : 'a t -> 'a t -> 'a t 
    (* 使い方：inter set1 set2 *) 
    (* 集合 set1 と set2 の共通部分を返す *) 
 
  val diff : 'a t -> 'a t -> 'a t 
    (* 使い方：diff set1 set2 *) 
    (* 集合 set1 から set2 の要素を取り除いた集合（差集合）を返す *) 
 
  val mem : 'a -> 'a t -> bool 
    (* 使い方：mem element set *) 
    (* 要素 element が集合 set に入っているかどうかを真偽値で返す *) 
end 
 
(* 集合のモジュール：リスト版 *) 
module Set : Set_t = struct 
  (* 要素の型が 'a の集合の型。ここではリストを使っているが *) 
  (* 木などを使うと以下の操作をより効率的に実現できるようになる *) 
  type 'a t = 'a list 
 
  (* 空集合 *) 
  (* empty : 'a t*) 
  let empty = [] 
 
  (* 目的：要素が element ひとつからなる集合を返す *) 
  (* singleton : 'a -> 'a t *) 
  let singleton element = [element] 
 
  (* 目的：集合 set1 と set2 の和集合を返す *) 
  (* union : 'a t -> 'a t -> 'a t *) 
  let union set1 set2 = set1 @ set2 
 
  (* 目的：集合 set1 と set2 の共通部分を返す *) 
  (* inter : 'a t -> 'a t -> 'a t *) 
  let inter set1 set2 = 
    List.fold_left (fun lst element -> 
		      if List.mem element set2 then element :: lst else lst) 
		   [] set1 
 
  (* 目的：集合 set1 から set2 の要素を取り除いた集合（差集合）を返す *) 
  (* diff : 'a t -> 'a t -> 'a t *) 
  let diff set1 set2 = 
    List.fold_left (fun lst element -> 
		      if List.mem element set2 then lst else element :: lst) 
		   [] set1 
 
  (* 目的：要素 element が集合 set に入っているかどうかを真偽値で返す *) 
  (* mem : 'a -> 'a t -> bool *) 
  let mem element set = List.mem element set 
end 
