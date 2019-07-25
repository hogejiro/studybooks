(* 本に関する情報を格納するレコード *) 
type book_t = { 
  title : string;	(* タイトル *) 
  author : string;	(* 著者 *) 
  publisher : string;	(* 出版社 *) 
  price : int;		(* 値段 *) 
  isbn : string;	(* ISBN *) 
} 
 
let book1 = { 
  title = "プログラミングの基礎"; 
  author = "浅井健一"; 
  publisher = "サイエンス社"; 
  price = 2300; 
  isbn = "978-4-7819-1160-1"; 
} 
 
let book2 = { 
  title = "アルゴリズムとデータ構造"; 
  author = "石畑清"; 
  publisher = "岩波書店"; 
  price = 3900; 
  isbn = "4-00-010343-1"; 
} 
 
let book3 = { 
  title = "プログラミング言語 Standard ML 入門"; 
  author = "大堀淳"; 
  publisher = "共立出版"; 
  price = 3800; 
  isbn = "4-320-12024-8"; 
} 
