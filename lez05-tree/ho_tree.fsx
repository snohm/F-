type 'a binTree =
    | Leaf                                     // empty tree
    | Node of 'a  * 'a binTree * 'a binTree    // Node(root, left, right)

let t13 = Node ( 13 , Leaf, Leaf )    // foglia
let t12 = Node ( 12 , Leaf, Leaf )    // foglia
let t11 = Node ( 11 , Leaf, Leaf )    // foglia
let t10 = Node ( 10, Leaf,  t13 )  
let t9 = Node ( 9, Leaf, t12 )
let t8 = Node ( 8, t11, Leaf ) 
let t7 = Node ( 7, Leaf,  t10 )  
let t6 = Node( 6, t9, Leaf) 
let t5 = Node ( 5 , t7, t8 )
let t4 = Node ( 4 , Leaf, Leaf )  // foglia
let t3 = Node( 3, t5, t6 ) 
let t2 = Node ( 2, Leaf, t4 ) 
let t1 = Node ( 1, t2, t3 ) 

(* HO su un albero:

Analogamente alla funzione List.map.

Dati

  -  una funzione f : 'a -> 'b
  -  un albero binario tree :'a binTree

costruisce l'albero ottenuto applicando f a tutti i nodi dell'albero tree.

*)   

// mapTree : f:('a -> 'b) -> tree:'a binTree -> 'b binTree
let rec mapTree f tree =
    match tree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
             Node ( f r, mapTree f left, mapTree f right )

// Esempio

let ex1 = mapTree (fun x -> x * x * x)  t8;;


(*

   filterToList : ('a -> bool) -> 'a binTree -> 'a list

tale che, dati:

- un predicato  pred : 'a -> bool
- una albero tree : 'a binTree

   filterToList pred tree

visita l'albero tree in inorder e costruisce la lista
degli elementi visitati che soddisfano il predicato pred.

*)

let rec filterToList  pred tree  =
    match tree with
        | Leaf -> []
        | Node ( r, left, right ) ->
            let filterL = filterToList  pred left
            let filterR =  filterToList  pred right
            if pred r then  filterL @ (r :: filterR)
            else  filterL  @  filterR

// Esempio

let t1even = filterToList  (fun n -> n % 2 = 0) t1  
// [2; 4; 10; 8; 12; 6]


// E  se volessi una filter da alberi a **alberi** ? Che faccio se un nodo **non** soddisfa pred?
// La cosa più semplice è preservare la struttura del albero, ma restituire un None

//  filterToTree : pred:('a -> bool) -> tree:'a binTree -> ('a option) binTree
let rec filterToTree  pred tree  =
    match tree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            let filterL = filterToTree  pred left
            let filterR =  filterToTree  pred right
            if pred r then  Node(Some r, filterL, filterR)
            else  Node( None, filterL, filterR)

let Ot1small = filterToTree  (fun n ->  n < 5) t1 

(*******    FOLD SU ALBERI ******)

(*
E' possibile definire una funzione fold su alberi binari analoga a fold su liste.

*)

// Ripasso: Fold su liste (right fold)
// foldBack : f:('a -> 'b -> 'b) -> ls:'a list -> e:'b -> 'b
let rec foldBack f ls e =
   match ls with
     | [] -> e   
     | x :: xs -> f x (foldBack f xs e)


(*

La definizione  di foldTree su un albero di tipo 'a : binTree segue lo stesso schema:

- a Leaf e' associato un valore   e : 'b
- a Node (x, left, right)   e' associata una funzione

      f x vl vr  

  dove: 
  *  vl rappresenta il valore associato a left  (tipo 'b)
  *  vr rappresenta il valore associato a right (tipo 'b)
  Il valore di 'f x vl vr' ha tipo 'b, quindi:

        f : 'a -> 'b -> 'b -> 'b
  
La funzione foldTree ha tipo 

  foldTree :  f:('a -> 'b -> 'b -> 'b) -> e:'b -> tree:'a binTree -> 'b

Funzioni di questo tipo, che generalizzano fold a tipi di dati algebrici  (ricorsivi) arbitrari,
sono anche chiamate *catamorfismi (catamorphism)*.

*)   

let rec foldTree f e tree = 
  match tree with
    | Leaf -> e
    | Node (x, left, right) ->
        f x ( foldTree f e left )  ( foldTree f e right )
//            ^^^^^^^^^^^^^^^^^      
//                   vl                     vr

// Notate: è una foldBack -- la fold (left) normale, quella iterativa,
// non è esprimibile se non attraverso le continuazioni


// Ridefiniamo alcune delle funzioni viste sopra usando foldTree


// depthtTreeF : tree:'a binTree -> int
// calcola la profondita' di un albero
let depthTreeF tree = 
  foldTree (fun x vl vr ->  1 + max vl  vr )  0 tree

// inOrderF : tree:'a binTree -> 'a list
// visita inOrder
let inOrderF tree = 
  foldTree (fun x vl vr -> vl @ (x :: vr)) []  tree

let preOrderF tree = 
  foldTree (fun x vl vr -> x :: vl @ vr) []  tree

//  filterToListF : pred:('a -> bool) -> tree:'a binTree -> 'a list

let filterToListF pred tree =
    let f x vl vr = if pred x then  vl @ (x ::  vr)   else  vl @ vr
    foldTree f []  tree 


(*
Naturalmente, possiamo scrivere altre funzioni  alberi di espressione e per non perdere
la mano scriviamo il catamorfismo su expr
 *)

type expr =
  | V of string 
  | C of int
  | Sum of expr * expr
  | Diff of  expr * expr
  | Prod of  expr * expr  

let rec cata fv fc fs fd fp expr =
     match expr with
         | V x -> fv x
         | C n -> fc n
         |  Sum(e1,e2) -> fs (cata fv fc fs fd fp e1) (cata fv fc fs fd fp e2)
         |  Diff(e1,e2) -> fd (cata fv fc fs fd fp e1) (cata fv fc fs fd fp e2)
         |  Prod(e1,e2) -> fp (cata fv fc fs fd fp e1) (cata fv fc fs fd fp e2)

// profondità di AST per expr
let depth  =
    let mx  vl vr =  1 + (max vl  vr)
    cata (fun _ -> 1) (fun _ -> 1) mx mx mx

let a3 = C 10
let dd = depth a3

// un altro esempio è adattare la vista inOrder per stampare
// l'espressione con gli operatori infissi. Facile generare anche scritture prefisse o postfisse.


let infixExp =
    let printOp op t1 t2   =
        match op with
            | "+" -> sprintf "(%s + %s)" t1 t2 
            | "*" -> sprintf "(%s * %s)" t1 t2 
            | "-" -> sprintf "( %s - %s)" t1 t2
            | _ -> failwith "does not happen"     

    cata 
      string 
      string 
      (printOp "+")
      (printOp "-")
      (printOp "*")

let ss3 = infixExp a3;;

// esempio finale: generazione di codice intermedio

type instr =
  | ADD
  | MULT
  | DIFF
  | PUSH of int
  | PUSHV of string



let rec trans e    =
    match e with
    | V x ->  [PUSHV x]
    | C n -> [PUSH n]
    | Sum(e1,e2)   -> trans e1  @  trans e2  @ [ADD]
    | Diff(e1,e2)  -> trans e1   @ trans e2  @ [DIFF]
    | Prod(e1,e2)  -> trans e1   @ trans e2  @ [MULT]

let pp = trans a3

let transCata =    
    cata 
      (fun x  -> [PUSHV x])
      (fun x  -> [PUSH x])
      (fun e1 e2 -> e1 @ e2 @ [ADD])
      (fun e1 e2 -> e1 @ e2   @ [DIFF])
      (fun e1 e2 -> e1 @ e2   @ [MULT])

let pp1 = transCata a3
