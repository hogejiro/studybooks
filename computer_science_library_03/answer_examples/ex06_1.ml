(* 
# let square x = x * x ;; 
val square : int -> int = <fun> 
# square 3. ;;				(* 3. ではなく 3 が正しい *) 
         ^^ 
This expression has type float but is here used with type int 
# let circle = 2. *. pi *. r ;;		(* circle の後に r が足りない *) 
                     ^^ 
Unbound value pi 
# circle 2. ;;				(* 上でエラーになったため *) 
  ^^^^^^				(* circle が定義されていない *) 
Unbound value circle 
# let TV bangumi youbi =		(* 大文字の関数は定義できない *) 
                 ^^^^^ 
    bangumi ^ "は" ^ youbi ^ "に放映です。" ;; 
Syntax error 
# TV "ポケモン" "木曜日" ;;		(* 上でエラーになったため *) 
                ^^^^^^^^		(* 関数が定義されていない *) 
Syntax error 
# 
*) 
