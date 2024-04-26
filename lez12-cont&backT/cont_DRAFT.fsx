(*

Continuation Passing Style (CPS) 
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


This is a very general technique that adds a function as an extra argument to the given procedure, 
which describes "the rest of the computation to be performed". 
The main function does not return, instead it builds the stack of function calls
("Dont't call us, we'll cal call you" ....)

Continuations are very useful in making the flow of control explicit.

Suppose we have a function

  f : 'a -> 'b

You can apply f to a term t of type 'a  and get a value v of type 'b,
then you can apply some other  actions on v so to get a final result res of type 'r.

Let k be the functione tha, given v, yields res;
note that k is a function of type 'b -> 'r


    t  |--- f --->  v  |--- k --->  res 
   'a              'b               'r 
                    ^^^^^^^^^^^^^^^^^
                       continuation 

The  CPS version of f is obtained by passing k as an extra argument.
This argument k represents the remaining computation that should be performed  
when the call 'f t' has produced the value v; k is called a *continuation*. 

The CPS version of f is a function f_cps of type

    f_cps  :  'a -> ('b -> 'r) -> 'r      // where 'r is any type

Given a term t : 'a and a continuation k : 'b ->'r, we have: 

 f_cps t k =  k (f t)

-----------------------------

We start with a trivial example.

The CPS version of the function

     sum : int -> int -> int  // sum of two integers
            x      y 

is the function  
  
  sum_cps : int -> int -> (int -> 'r) -> 'r
             x      y         k

defined below.

*)

// sum_cps : x:int -> y:int -> k:(int -> 'r) -> 'r
let sum_cps x y k =
    k ( x + y )

// Examples of applications of sum_cps

// f : x:int -> int
let f x = 2 * x + 10 

// we use the function  f as continuation
let n1 = sum_cps 1 2 f  // 16

let n2 = sum_cps 1 2 (fun res -> f ( 2 * res) )  
// the parameter res will be instantiated with 3 (the result of 1 + 2)

(*
   n2 =  sum_cps 1 2 (fun res -> f ( 2 * res) )
      = ( fun res -> f ( 2 * res) ) (1 + 2)
      = ( fun res -> f ( 2 * res) ) 3  // now res is instantiated with 3
      = f ( 2 * 3 )
      = f 6
      = 22
 
*)

let f1 = sum_cps 1 2 ( fun res ->  float res / 2.0 ) 
// val f1 : float = 1.5
// The continuation has type int -> float

// printRes : x:int -> unit
let printRes x = printfn  "The result is %d "  x     


sum_cps 1 2 printRes
// The result is 3 
// The continuation has type int -> unit

(*

If we are only interested in computing the sum of two integers,
we can call sum_cps with the identity function id as continuation.

*)

let n3 = sum_cps 1 2 id // 3

(*

Let us say that an integer x is a *small natural (smallNat)* iff 0 <= x <= 50.

We define a function sumSmallnat_cps (in CPS style) wich operates on smallNat's.
Let x and y be two integers:

- if both  x, y and the sum x+y are smallNat, 
  then sumSmallnat_cps computes x+y and then call the continuation.
- Otherwise, an error is reported, which must be handled a part.

To treat the second case, we introduce an auxiliary continutation, we call handler.

We represent errors by strings, thus the handler is a continuation of type

  handler :  string -> 'r

To sum up, the function sumSmallNat_cps has type:

  sumSmallNat_cps : int -> int -> (int -> 'r) -> (string -> 'r) -> 'r
                     x      y     ^^^^^^^^^^^    ^^^^^^^^^^^^^^  
                                    regular        handler 
                                    cont.          cont.

Note that the two continuations must have the *same* result type 'r 

*)



//isSmallNat : x:int -> bool
// Check if the integer x is a smallNat
let isSmallNat x = (0 <= x) && (x <= 50)

// sumSmallNat_cps : x:int -> y:int -> k:(int -> r) -> handler:(string -> r) -> r
let sumSmallNat_cps x y k handler =
    if not ( isSmallNat x ) then  // x is not a smallNat
        let err_x =  "SUM: argument " + string x + " is not a smallNat"
        handler err_x
    else if not ( isSmallNat y ) then   // y is not a smallNat     
        let err_y =  "SUM: argument " + string y + " is not a smallNat"
        handler err_y
    else // x and y are smallNat
        let sum = x + y 
        if not ( isSmallNat sum ) then  // x+y is not a smallNat
            let err_sum =  "SUM: " + string x + " + " + string y + " is not a smallNat"   
            handler err_sum
        else // x and y and x + y are smallNat
          k sum    // regular continuation

// printErr : err:string -> unit
let printErr err = printfn "** ERROR ==> %s" err
 
sumSmallNat_cps 1 2 printRes printErr
// The result is 3 

sumSmallNat_cps 1 -2 printRes printErr
// ** ERROR ==> SUM: argument -2 is not a smallNat

sumSmallNat_cps 30 25 printRes printErr
// ** ERROR ==> SUM: 30 + 25 is not a smallNat

sumSmallNat_cps 10 20 (fun s -> printRes ( 10 * s) ) printErr
// The result is 300 

(*

Note that the call

   sumSmallNat_cps 1 2 id printErr

 is not well-typed.  Indeed:

 - the first continutation (id)  has type  int -> int
   
 - the second continuation (printErr) has type   string ->  unit

Thus, the result types of the two continuations are different.

*)

(*

EXERCISE
========

By exploiting sumSmallNat_cps,  define the function 

 sumSmallNatOpt : int -> int -> int option

such that

 sumSmallNatOpt x y = Some (x+y)    if x and y and  x+y are smallNat
                    = None          otherwise 

*)


// sumSmallNatOpt : x:int -> y:int -> int option
let sumSmallNatOpt x y = .... 


let sum1 = sumSmallNatOpt 1 2    // Some 3
let sum2 = sumSmallNatOpt 1 -2   // None 
let sum3 = sumSmallNatOpt 30 25  // None

(*

Composition of continuations is not trivial.

Let us assume to have the CPS functions:

   sum_cps  :   int -> int -> (int -> 'r) -> 'r    // sum of two integers
square_cps  :   int -> (int -> 'r) -> 'r           // square of an integer  

We define a CPS function

 sumSquares_cps :  int -> int -> (int -> 'r) -> 'r
                    x      x         k    

that, given two integers x and y, computes x^2 + y^2

We define sumSquares_cps by composing  sum_cps and square_cps. 

*)


// square_cps : x:int -> k:(int -> 'r) -> 'r
let square_cps x k = k ( x * x )

// sumSquares_cps : x:int -> y:int -> k:(int -> 'r) -> 'r
let sumSquares_cps x y  k =
    square_cps  x  ( fun sq_x ->        // sq_x will be instantiated by x^2 (the square of x)
        square_cps y  ( fun sq_y ->     // sq_y will be instantiated by y^2 (the square of y)
           sum_cps sq_x sq_y  k )
	  )

(*

 sumSquares_cps 2 3 k 
    =  
      square_cps  x  ( fun sq_x ->  square_cps y  ( fun sq_y ->  sum_cps sq_x sq_y  k ) )  2 3 k                

    =    
      square_cps  3  ( fun sq_x ->  square_cps y  ( fun sq_y ->  sum_cps sq_x sq_y  k ) ) 3 k 
    =  // sq_x --> 4 (= 2^2)

      square_cps y  ( fun sq_y ->  sum_cps 4 sq_y  k ) ) 3 k

    =      

      square_cps 3  ( fun sq_y ->  sum_cps 4 sq_y  k ) ) 3 k
      
    = // sq_y --> 9 // = 3^2 

     sum_cps 4 9  k  
     
    = k 13

*)



// example of application
sumSquares_cps 3 4 printRes
// The result is 25 

(*

We tweak the functions square_cps and sumSquares_cps so that they work on smallNat:
whenever a value outside smallNat is obtained, an error is reported.

We add the continuation handler to treat errors.

*)

// squareSmallNat_cps : x:int -> k:(int -> 'r) -> handler:(string -> 'r) -> 'r
let squareSmallNat_cps x  k handler =
    if not (isSmallNat x) then  // x is not a smallNat
        let  err_x =  "SQUARE: argument " + string x + " is not a smallNat"
        handler err_x
    else  // x is a smallNat
        let sq = x * x
        if not (isSmallNat sq) then  // x^2 is not a smallNat
            let  err_sq =  "SQUARE: " + string x + "^2 is not a smallNat"
            handler err_sq
        else  // x and x^2 are smallNat 
          k sq    // regular continuation

// example 
// sumSquaresSmallNat_cps : x:int -> y:int -> k:(int -> 'a) -> handler:(string -> 'a) -> 'a
squareSmallNat_cps 5 printRes printErr 

//sumSquaresSmallNat_cps : x:int -> y:int -> k:(int -> r) -> handler:(string -> r) -> r
let sumSquaresSmallNat_cps x y  k handler =
    ...

// examples 

sumSquaresSmallNat_cps 2 3 printRes printErr 
//The result is 13 

sumSquaresSmallNat_cps 2 100 printRes printErr 
// ** ERROR ==> SQUARE: argument 100 is not a smallNat
// Here the error has been reported by squareSmallNat

sumSquaresSmallNat_cps 30 1 printRes printErr 
// ** ERROR ==> SQUARE: 30^2 is not a smallNat
// Here the error has been reported by squareSmallNat

sumSquaresSmallNat_cps 5 6 printRes printErr 
// ** ERROR ==> SUM: 25 + 36 is not a smallNat
 // Here the error has been reported by sumSmallNat

(*

CPS and tail recursion
^^^^^^^^^^^^^^^^^^^^^^

It's a general result that any function can be translated
to a tail recursive one using CPS. However, no free lunch:
what you save in stack space,  you pay on the heap allocating closures.

We start with the factorial function.

*)

// Recursive definition of factorial
// fact : n:int -> int
// We *assume* n >= 0
let rec fact n =
  match n with
  | 0 ->   1                     // base step  (n=0) 
  | _ ->   n * fact (n - 1)      // inductive step (n>0)
//              ^^^^^^^^^^     
//                   v

(*
The function fact is not tail recursive. 
We can obtain a tail-recursive function 

 factC : int -> int    // factC n = n!, where n >= 0

by using CPS. 

We proceed as follows:

1) We define the CPS version of fact, namely the recursive function 

   fact_cps : int -> (int-> 'r) -> 'r 

The crucial property is that fact_cps is *iterative (tail recursion)*.

2) factC is obtained  by applying fact_cps using the idendity function id as continuation.

*)

//fact_cps : int -> (int-> 'r) -> 'r 
// We *assume* n >= 0
let rec fact_cps n  k = 
  match n with
  |0 ->   k 1
  | _ ->   fact_cps (n - 1) ( fun v -> k (n * v) )
//                            ^^^^^^^^^^^^^^^^^^
 
fact_cps 5 id 
// factC : int -> int 
// Main function to compute the factorial (iterative computation)
//  We *assume* n >= 0
let factC n = fact_cps 5 id 

 
(*
 
EXERCISES
=========

1) By tracing the computation, show that

 fact_cps 3 k = k 6

2) In the definition of factC, we have assumed that n >= 0.
Add a handler continuation to fatc_cps so to treat the case n < 0 and rewrite factC and fact_cps accordingly. 

*)

(*

We stress that CPS can be used to transform *any* recursive function into a tail recursive function,
moreover the transformation is effective (we can write an algorithm to perform it).

Next example based on binary trees are significant since tail recursion cannot be obtained using accumulators
(see the discussion in the lecture about tail recursion).

*)


// binary tree of integers
type IntBTree =
    | Leaf
    | Node of int * IntBTree * IntBTree

// sumTree : tr:IntBTree -> int
// sum the nodes of a tree
let rec sumTree tr =
    match tr with
        | Leaf -> 0
        | Node (n, left, right) -> sumTree left + sumTree right + n
//                                 ^^^^^^^^^^^^   ^^^^^^^^^^^^^ 
//                                    vLeft            vRight 

// CPS translation
(*
In the CPS translation, the inductive step is rewritten as follows:

(1) We perform a recursive call  to the left subtree
(2) The recursive call to the right subtree is embedded in the continuation function
    of  the recursive call (1)

*)



// sumTree_cps : tr:IntBTree -> k:(int -> 'r) -> 'r
// sumTree in CPS style
let rec sumTree_cps tr k =
    match tr with 
    | Leaf -> k 0
    | Node(n, left, right) -> sumTree_cps left (fun v1 -> 
                                      sumTree_cps right 
                                        (fun v2 -> 
                                          k (v1 + v2 + n )))
      

// Note that sumTree_cps is tail recursive
// The inner recursive call 'sumTree_cps  right ...'  is in the body of a function definition

//  sumTreeC : tr:IntBTree -> int
// main function to sum the nodes of a tree (iterative computation)
let sumTreeC tr = sumTree_cps tr id


(*
tr1

     1
   /    \
  2      3     
   \    /  \
    4  5    6
   
*)       

let tr1 = Node( 1,
                Node ( 2, Leaf,  Node (4, Leaf, Leaf) ) ,
                Node ( 3, Node(5,Leaf,Leaf) ,  Node(6,Leaf,Leaf) )
               )


let c1 = sumTree tr1 // 21

let c2 = sumTreeC tr1  // 21

(*

We consider the variant of summTree which only operates  on smallNat's;
whenever  a non-SmallNat is generated, an excetpion is thrown.
As usual, we introduce a handler to treat exceptional cases.

*)

// sumTreeSmallNat_cps : tr:IntBTree -> k:(int -> 'r) -> handler:(string -> 'r) -> 'r
let rec sumTreeSmallNat_cps tr  k handler =
    match tr with 
    | Leaf -> k 0
    |  Node( n, left, right) ->
        if not (isSmallNat n) then // the root n is not a smallNat
            let err_n = "SUMTREE: node " + string n + " is not a smallInt"
            handler err_n
        else // the root n is a samllNat
            sumTreeSmallNat_cps left ( fun vLeft ->
                  sumTreeSmallNat_cps right ( fun vRight -> 
                    let sum = vLeft + vRight + n 
                    let sum_str = string vLeft + " + " + string vRight + " + " + string n
                    if not (isSmallNat sum) then // the sum i not a smallNat
                        let err_sum = "SUMTREE: " + sum_str + " is not a smallNat" 
                        handler err_sum
                    else k sum  // regulat continuation
                    ) 
                    handler )  
                    handler   
           
// some examples
 
sumTreeSmallNat_cps tr1 printRes printErr 
// The result is 21 

(*
tr2

     1
   /    \
  2      300     
   \    /  \
    4  5    6
   
*)       

let tr2 = Node( 1,
                Node ( 2, Node (4,Leaf,Leaf) , Leaf) ,
                Node ( 300, Node(5,Leaf,Leaf) ,  Node(6,Leaf,Leaf) )
               )

sumTreeSmallNat_cps tr2 printRes printErr 
// ** ERROR ==> SUMTREE: node 300 is not a smallInt

(*
tr3

     1
   /    \
  2      30     
   \    /  \
    4  15    6
   
*)       

let tr3 = Node( 1,
                Node ( 2, Node (4,Leaf,Leaf) , Leaf) ,
                Node ( 30, Node(15,Leaf,Leaf) ,  Node(6,Leaf,Leaf) )
               )

sumTreeSmallNat_cps tr3 printRes printErr
// ** ERROR ==> SUMTREE: 15 + 6 + 30 is not a smallNat

(*

EXERCISES
==========

** EXERCISE 1 **

Consider a function that traverses a list and returns the shortest prefix of the list 
such that a predicate pred is false on the next element.  
If pred holds on all elements, we return None, denoting  failure (no such a prefix exists).

Example.

Let us consider the predicate isEven which checks if an integer is even

let isEven n =  n % 2 = 0

Then:

prefix isEven  [2;4;5;8]    ==>  Some [2; 4]    // isEven 2, isEven 4, NOT (isEven 5) 
prefix isEven  [2;4;8]      ==>  None           // isEven 2, isEven 4, isEven 8 -- no prefix  
prefix isEven  [1;2;3;4]    ==>  Some []        // NOT (isEven 1) -- empty prefix
 

An efficient way to implement prefix is to use CPS.

i) Define the function

  prefix_cps: pred: ('a -> bool) -> xs: 'a list -> k: ('a list -> 'b) -> handler: ('c list -> 'b) -> 'b

which corresponds to the cps version of prefix.
Note that we have two continuations:

-  k: ('a list -> 'b) is the regular continuation
-  handler: ('c list -> 'b)  is the continuation to be used in case of failure (no prefix exists)


ii) Define 

    prefix: pred: ('a -> bool) -> xs: 'a list -> 'a list option

by calling prefix_cps with proper continuations.

Examples:


let pref1 = prefix isEven  [2;4;5;8]  
//  Some [2; 4] 
let pref2 = prefix isEven  [2;4;8] 
// None
let pref3 = prefix isEven  [1;2;3;4] 
// Some []
let pref4 = prefix isEven  [2;10;1;4;5;8]  
//  Some [2; 10] 

_____________________________________________________________________

** EXERCISE 2 **

i) Define a  function

 sumTreeSmallNat : IntBTree -> int option

such that

 sumTreeSmallNat tr = Some sum      if all the nodes in tr are smallInt,
                                    and sum is the sum of the nodes 
                                    and sum is a smallInt

                      None        otherwise          


Define sumTreeSmallNat using recursion (do not use the CPS functions defined above).

ii) Redefine sumTreeSmallNat by calling sumTreeSmallNat_cps with suitable continuations.

iii) Use FsCheck to check that the two functions are equivalent.

___________________________________________________________________________________________

** EXERCISE 3 **

Let us consider integer expressions of the kind

type expr =
  | C of int    
  | Sum  of expr * expr    
  | Diff of expr * expr
  | Prod of expr * expr
  

and the following evaluation function

// eval : expr -> int
let rec eval e  =
    match e with
    | C n   -> n
    | Sum (e1,e2)   -> eval e1  + eval e2 
    | Diff (e1,e2)  -> eval e1  - eval e2 
    | Prod (e1,e2)  -> eval e1  * eval e2 

Define a tail recursive evaluation function evalC equivalent to eval.



*)