(* 座標を表すモジュール *) 
module Zahyou = struct 
  let x = 3.0   (* x 座標 *) 
  let y = 4.0   (* y 座標 *) 
 
  (* 目的：受け取った点 (a, b) から点 (x, y) までの距離を求める *) 
  (* kyori : float * float -> float *) 
  let kyori (a, b) = 
    sqrt ((x -. a) *. (x -. a) +. (y -. b) *. (y -. b)) 
end 
