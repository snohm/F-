
// int list with subtypes


type tm =
  K of int
  | Nil
  | Cons of tm * tm
  | Hd of tm
  | Tl of tm

type tp = INT | L | E | C 


// tpck : tm -> tp option
let rec tpck e  = None  // ** DEFINIRE

// TEST

let t1 = tpck Nil   // Some E    
let t2 = tpck (Cons( K 3, Nil))   // Some C       
let t3 = tpck (Cons( Nil , Nil))   // None 
let t4 = tpck (Hd   (Cons( K 3, Nil))  )  // Some INT
let t5 = tpck ( Tl  (Cons( K 3, Nil)) )  // Some L
let t6 = tpck ( Hd Nil )  // None

// evalo : tm -> tm option

let rec evalo e  = None   // ** DEFINIRE

// TEST

let e1 = evalo (Hd (Cons(( K 3),Nil)))  // Some (K 3)    
let e2 = evalo (Tl (Cons(K 3,Nil)))    // Some Nil
let e3 = evalo ( Hd (Tl (Cons(K 3,Nil)))) // None 
