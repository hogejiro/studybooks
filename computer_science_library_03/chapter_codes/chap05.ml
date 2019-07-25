(* 5.1 *) 
 
let yugu_jikyu = 980 ;; 
 
(* 5.2 *) 
 
if true then 1 else 2 ;; 
if false then 1 else 2 ;; 
if 1 < 2 then 3 else 4 ;; 
if 3 + 4 < 5 then 1 else 2 ;; 
(* if 1 then 2 else 3 ;; *) 
(* if 3 + 4 > 5 then 1 else "2" ;; *) 
 
(* 5.3 *) 
 
#use "sect05_3/kyuyo.ml" ;; 
 
(* 5.4 *) 
 
let kyuyo x = 
  kihonkyu + x * (if x < 30 then jikyu else yugu_jikyu) ;; 
 
#use "sect05_4/kyuyo.ml" ;; 
 
(* 5.5 *) 
 
#use "sect05_5/abs_value.ml" ;; 
 
(* 5.6 *) 
 
#use "sect05_6/kion1.ml" ;; 
#use "sect05_6/kion2.ml" ;; 
#use "sect05_6/kion3.ml" ;; 
 
(* 5.7 *) 
