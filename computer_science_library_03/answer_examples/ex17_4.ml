(* あらかじめ ex17_2.ml, ex17_3.ml を読み込んでおく必要あり *) 
#use "ex17_2.ml" 
#use "ex17_3.ml" 
 
(* 目的：日付けを受け取ってきたら星座を返す *) 
(* seiza : year_t -> seiza_t *) 
let seiza year = match year with 
    January (hi) -> if hi <= 19 then Capricorus else Aquarius 
  | February (hi) -> if hi <= 18 then Aquarius else Pisces 
  | March (hi) -> if hi <= 20 then Pisces else Aries 
  | April (hi) -> if hi <= 19 then Aries else Taurus 
  | May (hi) -> if hi <= 20 then Taurus else Gemini 
  | June (hi) -> if hi <= 21 then Gemini else Cancer 
  | July (hi) -> if hi <= 22 then Cancer else Leo 
  | August (hi) -> if hi <= 22 then Leo else Virgo 
  | September (hi) -> if hi <= 22 then Virgo else Libra 
  | October (hi) -> if hi <= 23 then Libra else Scorpius 
  | November (hi) -> if hi <= 21 then Scorpius else Sagittarius 
  | December (hi) -> if hi <= 21 then Sagittarius else Capricorus 
 
(* テスト *) 
let test1 = seiza (June (11)) = Gemini 
let test2 = seiza (June (30)) = Cancer 
let test3 = seiza (September (17)) = Virgo 
let test4 = seiza (October (7)) = Libra 
