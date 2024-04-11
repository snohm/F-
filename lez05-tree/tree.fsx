(*****   ALBERI BINARI   *****

Un albero binario i cui nodi hanno tipo 'a
puo' essere rappresentato usando il tipo polimorfo ricorsivo

    'a binTree 

cosi' definito:

*)

type 'a binTree =
    | Leaf                                     // empty tree
    | Node of 'a  * 'a binTree * 'a binTree    // Node(root, left, right)


(*

Il tipo 'a binTree possiede due costruttori:

- il costruttore 

         Leaf : 'a binTree

  (costruttore senza argomenti) che rappresenta l'albero vuoto di tipo 'a

- il costruttore

         Node : 'a * 'a binTree * 'a binTree -> 'a binTree


   che data la tripla 

           (x , left ,  right )     

   costruisce l'albero  


                     x         
                   /   \
                 left  right 


dove x e' la radice (tipo 'a), left e right sono i sottoalberi sinistro e destro di x (tipo 'a binTree).

Esempio di albero binario
-------------------------

L'albero binario di interi


       - 1 -
      /     \
     2       3
    / \     / \ 
   o   4   o   o        o : empty tree 
      / \
     o   o


e' rappresentato dal seguente termine di tipo 'int binTree'
                             
 Node( 1 , Node (2, Leaf , Node(4,Leaf,Leaf)) ,  Node(3, Leaf,Leaf)  )
           
Infatti:

-   Node(4,Leaf,Leaf) rappresenta il sottoalbero di radice 4  (albero che contiene solo il nodo 4)

-   Node (2, Leaf, Node(4,Leaf,Leaf))  rappresenta il sottoalbero di radice 2

-   Node(3,Leaf,Leaf)   rappresenta il sottoalbero di radice 3


*)

(*** Esempio di albero binario usato negli esempi ***)

(*   t1 

       -------------  1  --------------
      |                               |
      2                    ---------- 3 ---------
     / \                   |                     |
    o   4           ------ 5 ------              6
       / \          |             |            /   \ 
      o   o         7             8           9     o
                  /  \           / \         /  \
                 o   10        11   o       o   12
                    /  \       /\               /\
                   o    13    o  o             o  o  
                        /\
                       o  o
 
*)  

// definizione di t1 
// tk e' il  sottoalbero di t1 avente come radice il nodo k

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

(*
Vi sono, naturalmente, molte altre varianti della nozione di
albero, alcune delle quali vedremo nella seconda parte della lezione. In
particolare, Expression trees, aka abstract syntax tree (AST) *)




(***** FUNZIONI SU ALBERI BINARI   *****)


// profondità di un albero = lunghezza del ramo più lungo
// : tree:'a binTree -> int
let rec depthTree tree =
    match tree with
        | Leaf -> 0
        | Node(x,l,r) -> 1 + max (depthTree l)(depthTree r)    

let d1 = depthTree t1

//  countNodes : tree:'a binTree -> int
//  conta i nodi di un albero

let rec countNodes tree =
  match tree with
    | Leaf -> 0
    | Node (_, left, right) -> 1 + countNodes left + countNodes right

// conta le foglie
// : tree:'a binTree -> int    
let rec countLeaves tree =
  match tree with
    | Leaf -> 1
    | Node (_, left, right) -> countLeaves left + countLeaves right

// 
    
(*

più efficiente contarle insieme, nodi e foglie, dove:
- nodes   e' il numero totale dei nodi dell'albero;
- leaves  e' il numero totale delle foglie dell'albero.
*)

let rec count tree =
    match tree with
        | Leaf -> (0,1)
        | Node (r, left, right) ->
          let (nodesL, leavesL) = count left
          let (nodesR, leavesR) = count right
          (nodesL + nodesR + 1, leavesL + leavesR) 


(* 
  Visita di un albero binario
^^^^^^^^^^^^^^^^^^^^^^^^^^^

La *visita* di un albero binario consiste nell'attraversare tutti i suoi nodi.

Ci sono tre modalita' principali di attraversamento di un albero binario,
che differiscono nell'ordine in cui i nodi sono visitati:

- visita *preorder*.
  Ordine di visita: radice, visita preorder sottoalbero sin,  visita preorder sottoalbero dx
   
- visita *inorder*.
  Ordine di visita: visita inorder sottoalbero sin, radice,  visita inorder sottoalbero dx
 
- visita *postorder*.
  Ordine di visita: visita postorder sottoalbero sin, visita postorder sottoalbero dx, radice

                         *)

let rec preOrder tree =
  match tree with
    | Leaf -> []
    | Node ( r, left, right ) ->
        r :: preOrder left @  ( preOrder right)

let l0 = preOrder t5 
// [5; 7; 10; 13; 8; 11]


let rec inOrder tree =
  match tree with
    | Leaf -> []
    | Node ( r, left, right ) ->
        inOrder left @  ( r :: inOrder right)

// Esempio
let l1 = inOrder t5 
// [7; 10; 13; 5; 11; 8]

let rec postOrder tree =
  match tree with
    | Leaf -> []
    | Node (r, left, right) ->
        postOrder left @ postOrder right @ [r]

let l2 = postOrder t5
//  [13; 10; 7; 11; 8; 5]

// visita via stampa: nota codominio: tree: 'a binTree -> unit
// nota sequencing, che è un let anonimo con target unit
let rec inOrderP tree =
  match tree with
    | Leaf ->  printf "\n"
    | Node ( r, left, right ) ->
        inOrderP left;
        printf "%A\t" r;
        inOrderP right       


// Le funzione che costruiscono liste sono quadratica per colpa di append: diventano lineare
// con un accumulatore -- vedremo la tecnica in generale più avanti

let inorderF btree =
    let rec inOrderFaster btree acc =
        match btree with
            | Leaf -> acc
            | Node ( r , left, right ) ->
                inOrderFaster left  (r :: (inOrderFaster right acc))
    inOrderFaster btree  []


// ALBERI BINARI DI RICERCA


(* Invariante dei search tree:

ogni Node(x,tl,tr) è tale che

1. x' < x se x' in tl (sottoalbero a sinistra)
1. x' > x se x' in tr (sottoalbero a destra)

Invariante è **implicita**

Un search tree  offre (almeno) le seguenti operazione

- inserimento (preservando invariante)

- test di appartenza:

questa è tipicamente efficiente in quanto proporzionale alla
profondità dell'albero (logartimica se albero è bilanciato),
diversamente dallo stesso test nelle liste (alla peggio lineare).

Per questo, alberi (bilanciati) sono usati per rappresentare insiemi,
mappe e dizionari -- in questo caso 'a è istanziato con il cartesiano
(key * value) e le operazioni sono lievemente differenti.

   *)


// Inserisce l'elemento x nell'albero binario di ricerca tree insert :
// 'a -> 'a Tree -> 'a Tree when 'a : comparison --- si noti la type
// constraint 'a : comparison, richiesto dall'uso dell'ordinamento
// sugli elelementi dell'albero
   
let rec insert  x  tree  =
    match tree with
        | Leaf -> Node(x, Leaf, Leaf)  // albero che contiene solo il nodo x 
        | Node(r, left, right) ->  
            if  x = r  then tree // nessuna duplicazione
            else if x < r then   Node(r,  insert  x left , right ) 
            else   Node(r , left, insert x right ) 

// Inserisce nell'albero di ricerca tree tutti gli elementi della lista list

let rec insertFromList tree list =
    match list with
    | [] -> tree
    | x :: xs ->
        let updatedTree = insert x tree
        insertFromList updatedTree xs

// Esempi

let intList = [ 20 ; 10 ; 60 ; 15 ; 40 ; 100 ; 30 ; 50 ; 70 ; 35 ; 42 ; 58 ; 75 ; 32; 37]

let intTree = insertFromList  Leaf intList

(*   inTree

      ------------- 20 --------------
      |                             |
     10                  --------- 60 ----------
    /  \                 |                      |
   o   15           ----- 40 ------            100
      /  \          |             |            /  \ 
     o    o        30            50           70   o
                  /  \           / \         /  \
                 o   35        42   58      o   75
                    /  \       /\   /\          /\
                   32   37    o  o o  o        o  o 
                   /\   /\
                  o  o o  o
 
    o : Leaf

*)  


let strList1 = ["pesca" ; "banana" ; "uva" ; "albicocca" ; "nocciola" ; "ribes" ]

let strTree1 = insertFromList  Leaf strList1 

(*   strTree1

                  -------------------- pesca --------------------
                  |                                             |
        ------ banana --------                                 uva
       |                      |                               /   \   
   albicocca               nocciola           ribes    o
      /\                    /    \                         / \
     o  o                  o      o                       o   o
        

*)

// esempio di dizionario
let dict = [("p",2) ; ("b",1) ; ("u",5) ; ("a" ,-2) ; ("n",42)  ]

let dictTree = insertFromList  Leaf dict 
    
//-----------------------------------------------------------------------


// Dato x e un albero binario di ricerca tree, restituisce true se e solo se x e' nell'albero
// search : 'a ->'a Tree -> bool when 'a : comparison

let rec search  x tree =
    match tree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) || ( x < r && search  x left ) ||  ( x > r && search x right )

           (*  Le condizioni  x < r e x > r servono per fare in modo che 
                le chiamate   siano disgiunte, ma l'ultima non è necessaria
                data la natura cronologica del pattern matching
           *)   



let y = search "banana" strTree1

// Nota: per i dizionari search va implementata passando la chiave e
// ritornando il valore associato.

// --------------------
                    
// La questione del bilanciamento:
// cosa succede se inserisco una lista già ordinata?
  
let nt =  insertFromList Leaf [1..10]

// calcolate la profondità ... E' un albero degenere (è
// sostanzialmente una lista) e come tale non è più utile come search tree.

// Risposta: usare una nozione di albero autobilanciante (AVL, red-black,etc)

(*

Un albero binario è bilanciato se per ogni nodo vale che il numero di
   nodi interni nel sottoalbero sinistro e il numero di nodi interni
   nel sottoalbero destro differiscono al massimo di 1.  *)

// definizione  poco efficiente O(n^2)
let rec is_balanced = function
    Leaf -> true
    |Node(_,l,r) ->
        (countNodes l - countNodes r) |> abs <= 1 &&
        is_balanced l &&
        is_balanced r

