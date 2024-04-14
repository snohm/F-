type 'a sexp = Atom of 'a | Pair of ('a sexp * 'a sexp)

let isa = Pair(Atom "is",Atom "a")
let asexp = Pair(Atom "s",Atom "expression")  
  
let thisisasexp = Pair(Atom "this", Pair(isa,asexp))




// let rec smap f xs = 

let test = smap  (fun (s : string) -> s + s) thisisasexp



// serialize :'a list -> string sexp)   
let rec serialize (ys : 'a list) = []


let s1 =  serialize [1;2] 
// val s1 : string sexp =
// Pair
 //   (Atom "cons",
 //    Pair (Atom "1", Pair (Atom "cons", Pair (Atom "2", Atom "nil"))))

// unserialize : string sexp -> string list
let rec unser ss = []

let u1 = unser s1
//val u1 : string list = ["1"; "2"]
#r "../FsCheck"
open FsCheck


let ser_p (xs : string list ) = true
 
do Check.Quick ser_p



// let rec sfoldB fp fa ss = 
  

let count s = 9


