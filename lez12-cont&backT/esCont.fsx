let isSmallNat x = (0 <= x) && (x <= 50)

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


(*

EXERCISE
========

By exploiting sumSmallNat_cps,  define the function 

 sumSmallNatOpt : int -> int -> int option

such that

 sumSmallNatOpt x y = Some (x+y)    if x and y and  x+y are smallNat
                    = None          otherwise 

*)

let sumSmallNatOpt x y = sumSmallNat_cps x y (fun s -> Some s) (fun _ -> None)

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

