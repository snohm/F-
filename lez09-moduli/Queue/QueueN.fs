// Naive implementation of Queue

module Queue

exception EmptyQueue

type Queue<'a> =  Q of  'a list  // *deve* essere un tagged value

(*
  il primo elemento della lista e' il primo elemento della coda
  (prossimo elemento da togliere)
 *) 
 


let empty = Q [] // coda vuota
// val empty : Queue<'a>

let get  (Q qs) =
// l'elemento da togliere e' il primo della lista qs  
  match qs with
    | [] ->   raise EmptyQueue
    | x :: xs ->  (x , Q xs)
// val get : Queue<'a> -> 'a * Queue<'a>


  
// y va inserito dopo l'ultimo elemento della lista qs
//  val put : 'a -> Queue<'a> -> Queue<'a>
let  put y (Q qs) = Q (qs @ [y])

// nota: operazione lineare in |qs|



