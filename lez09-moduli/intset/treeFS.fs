module  IFSet

// we use binary search trees as an alternative more efficient implementation of sets
// HR, page 148 etc

// However, they are *not* self balancing as in the Set library

type IFSet = Leaf | Node of int * IFSet * IFSet

let empty =   Leaf

let isEmpty t =
    (t = Leaf)

let rec contains  x  btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) || 
            ( (x < r) && contains  x left ) ||
            ( (x > r) && contains  x right );;  

let rec add  x  btree  =
    match btree with
        | Leaf -> Node(x, Leaf, Leaf)  
        | Node(r, left, right) when x = r ->  btree 
        | Node(r, left, right) when x < r ->  Node(r,  (add  x left) , right )
        | Node(r, left, right)  ->  Node(r , left, (add x  right) ) 


// 
let rec union  s1 s2 = 
    match s1 with
     | Leaf -> s2
     | Node(x,ltr,rtr) ->
         let ts = add x s2
         let tsl = union  ltr ts
         union  rtr tsl;;
        
let ofList xs =
    List.fold (fun t x -> add x t ) Leaf xs

// inorder visit
let toList btree =
    let rec inOrderFaster btree acc =
        match btree with
            | Leaf -> acc
            | Node ( r , left, right ) ->
                inOrderFaster left  (r :: (inOrderFaster right acc))
    inOrderFaster btree  []

(*    
let rec toList_slow tree  =
    match tree with
        | Leaf -> []
        | Node ( r, left, right ) ->
            toList left @  [r] @ toList right
  *)  

