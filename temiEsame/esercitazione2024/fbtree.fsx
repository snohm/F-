type  'a fbtree = Node of 'a * (('a fbtree) list)

let tree1 
  = Node(0,
         [ Node(1, 
               [ Node(2,[]) ;
                 Node(3,[]) ;
                 Node(4,[]) ;
                 Node(5,[]) 
               ]) ;
           Node(6,[]) ;
           Node(7,
               [
                Node(8, [Node(9,[])]) ;
                Node(10,[])  
               ])
          ])  


let tree2 = 
     Node(0,
          [ Node(1, [ Node(2,[]) ; Node(3,[]) ]) ; 
            Node(4, [ Node(5,[]) ; Node(6,[]) ; Node(7,[]) ]) ;    
            Node(8, [])
          ])      
      

let tree3 = 
     Node(0,
          [ Node(1, [Node(2,[])]) ;
            Node(3,
               [ Node(4, [Node(5,[]); Node(6,[])]) ;
                 Node(7,[]) ;
                 Node(8,[]) ;
                 Node(9,[]) ;
                 Node(10,[])
               ])      
          ])

// 1) count : 'a fbtree -> int
//che conta i nodi di un albero.
let ((a:int),(tree: 'a fbtree)) = tree1
let count fbtree = 
    match fbtree with 
    | Node(_, []) -> 1
    | Node(_, )

