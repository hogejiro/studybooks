(* 目的：組で表された平面座標を受け取ったら x 軸に対称な点の座標を返す *) 
(* taisho_x : float * float -> float * float *) 
let taisho_x point = match point with 
  (x, y) -> (x, -. y) 
 
(* -. y は 0. -. y でも同じ *) 
 
(* テスト *) 
let test1 = taisho_x (0.0, 0.0) = (0.0, 0.0) 
let test2 = taisho_x (2.3, 5.1) = (2.3, -5.1) 
let test3 = taisho_x (-3.8, -2.4) = (-3.8, 2.4) 
