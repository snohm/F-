

// typed
type 'a tsexp = Atom of char * 'a | Pair of ('a tsexp * 'a tsexp)

let isat = Pair(Atom('s', "is"),Atom('s', "a"))
let onetwo = Pair(Atom('i', 1),Atom('i', 2))
let ill = Pair(Atom('i', "1"),Atom('s', "1"))

type ty = INT | FLOAT | STRING 


let rec tyck = None

let t1 = tyck isat
let t2 = tyck onetwo
let t3 = tyck ill
