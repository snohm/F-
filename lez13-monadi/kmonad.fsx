

// https://stackoverflow.com/questions/40052256/how-does-continuation-monad-really-work/42062682#42062682

// For semplicity, we start with continuations in unit. See at the end
// for the general case

/// A continuation is a function that represents "the rest of the computation",
/// here just performing a side effect.

module SimpleContMonad =
    type Cont<'T> = ('T -> unit)

/// An incomplete computation is a function which, when given a continuation,
/// will return a computation. This the basis of the continuation monad.
    type Inc<'T> = Cont<'T> -> unit

/// Creates an incomplete computation that holds the given value.
    let ret t : Inc<'T> =
        fun (k : Cont<'T>) -> k t

/// Composition of incomplete computations.
    let bind (incT : Inc<'T>) (wrap : 'T -> Inc<'U>) : Inc<'U> =
        fun (contU : Cont<'U>) ->   // return an Inc, which is a function that takes a continuation as input
            incT (fun (t : 'T) ->   // force the given incomplete computation to cough up its wrapped value
            (wrap t) contU)        // re-wrap the raw value so it can be sent to the given continuation


      
/// Monad definition.
    type ContinuationBuilder() =
        member __.Return(value) = ret value
        member __.Bind(inc, wrap) = bind inc wrap

/// Builder instance.
    let cont = ContinuationBuilder()

/// initial continuation    
    let initK x =  printf "res: %A\n" x

// standard factorial using explicit continuations
    let factK n = 
        let rec floop n K =
            match n with 
            | 0 -> K 1
            | n -> floop (n - 1) (fun r -> K (n * r))
        floop n (initK )   

// cont-factorial
  
    let factIt n =
        let rec floop n  =  
              cont {
                match n with 
                | 0 -> return  1
                | n -> let! res = floop (n - 1)
                       return n * res}
        floop n initK 

/// Continuation-ized version of Fibonacci function.
    let fibC n = 
        let rec fib n =
            cont {
            match n with
            | 0 -> return 0
            | 1 -> return 1
            | n ->
                let! x1 = fib (n - 1)
                let! x2 = fib (n - 2)
                return x1 + x2
        }
        fib n initK

          
// Now, we go for the general case

type Cont<'T, 'U> = ('T -> 'U)

type Inc<'T, 'U> = Cont<'T, 'U> -> 'U

let ret (t : 'T) : Inc<'T, _> =
    fun (k : Cont<'T, _>) -> k t

let bind (incT : Inc<'T, _>) (wrap : 'T -> Inc<'U, _>) : Inc<'U, _> =
    fun (contU : Cont<'U, _>) ->   // return an Inc, which is a function that takes a continuation as input
        incT (fun t ->             // force the given incomplete computation to cough up its wrapped value
            (wrap t) contU)        // re-wrap the raw value so it can be sent to the given continuation


//  action is on first parameter           
// Monad definition.
type ContinuationBuild() =
    member __.Return(value) = ret value
    member __.Bind(inc, wrap) = bind inc wrap

// Builder instance.
let continuation = ContinuationBuild()

// same with factorial
let factIt n =
        let rec floop n  =  
              continuation {
                match n with 
                | 0 -> return  1
                | n -> let! res = floop (n - 1)
                       return n * res}
        floop n id
         

// Continuation-ized version of Fibonacci function T = U =  int.
let fibC n = 
    let rec fib n  =
        continuation {
        match n with
            | 0 -> return 0
            | 1 -> return 1
            | n ->
                let! x1 = fib (n - 1)
                let! x2 = fib (n - 2)
                return x1 + x2
        }
    fib n id



// iterative append 

let appInt xs ys = 
    let rec loopApp xs ys =
        continuation {
        match xs with
            | [] -> return ys
            | x :: xs' -> 
            let! r = loopApp xs' ys  
            return (x :: r )
        }
    loopApp xs ys id 

// trees

type BinTree<'a> = 
        | Leaf
        | Node of BinTree<'a> * 'a *  BinTree<'a>

// this is tail recursive
let rec countC t  =
    continuation {
            match t with
            | Leaf          -> return 0
            | Node(tl,n,tr) ->
                let! vl = countC tl
                let! vr = countC tr
                return vl+vr+1
                }
type expr =
    | C of int    
    | Sum  of expr * expr    
    | Diff of expr * expr
    | Prod of expr * expr

// and this as well                                    
let evK e =
    let rec loop e  =
            continuation {
            match e with
                C m -> return  m
                |Sum(e1,e2) -> 
                    let! n1 = loop e1
                    let! n2 = loop e2
                    return n1 + n2
                |Diff(e1,e2) -> 
                    let! n1 = loop e1
                    let! n2 = loop e2
                    return n1 - n2
                |Prod(e1,e2) -> 
                    let! n1 = loop e1
                    let! n2 = loop e2
                    return n1 * n2                                        
                                }
    loop e id

