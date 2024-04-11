// Other tree structures: note the use of modules and indentation to
// reuse the same names.
// As a sample function, we use mirror, which swaps trees
   
// just the skeleton of the tree (no values)
module skeleton =
    type binTree =
    | Leaf                                    
    | Node of binTree * binTree    // Node( left, right)

    let rec mirror = function
        |Leaf -> Leaf
        |Node(l,r) -> Node(mirror r, mirror l)


// values stored in leaves, not in nodes
module val_in_leaf =
    type 'a binTree =
    | Leaf  of 'a                                  // leaf with content
    | Node of 'a binTree * 'a binTree    // Node(left, right)

    let rec mirror = function
        |Leaf x -> Leaf x
        |Node(l,r) -> Node(mirror r, mirror l)


// different kind of values stored both in leaves and nodes
module augmented  =
    type  binTree<'a,'b> =
    | Leaf  of 'a                                  // 'a Leaf
    | Node of 'b * binTree<'a,'b> * binTree<'a,'b>    // Node('b root, left, right)

    let rec mirror = function
        |Leaf x -> Leaf x
        |Node(x,l,r) -> Node(x, mirror r, mirror l)

// 1-2 trees
(* 
      - 1-
    /      \ 
   2        8
   |        |  
   3        9  
  / \       | 
 4   5      10
     |      |
     6      11
     |     /  \   
     7    12  13
               |
              14 
*)  

module onetwo = 
    type 'a binTree =
        | Leaf                                     // empty tree
        | Node1 of 'a  * 'a binTree     // single child
        | Node2 of 'a  * 'a binTree * 'a binTree    // Node(root, left, right)
    
    let rec mirror = function
        |Leaf -> Leaf
        |Node1(x,tr) -> Node1(x,mirror tr)
        |Node2(x,l,r) -> Node2(x, mirror r, mirror l)


// finitely branching trees
module rose =
    type 'a tree = Node of 'a * ('a tree list)

    let rec  mirror (Node(x,ts)) = Node(x,List.map mirror ts)


// infinitely branching trees -- we'll see sequences in a next lecture
module infrose =
    type 'a tree = Node of 'a * (seq<'a tree>)

    let rec mirror (Node(x,ts)) = Node(x,Seq.map mirror ts)    


