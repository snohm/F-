// how to test ADT symbolically

#r "nuget:FsCheck"
open FsCheck
#r "QueueN/bin/Debug/net6.0/QueueN.dll"

open Queue

// simplification for spec checking, just returning the rest of the queue
let getQ q =
    let (_,rq) = get q
    rq

// some algebraic properties of queues
    // get (put x emp) = emp
// We can check this as we just need to generate an element
let p_e (n : int) =
    (put n empty |> getQ) = empty

do Check.Quick p_e

// But if try to generalize it to any queue, we run into problems
// get (put n q)  = q

let p_ge q (n : int) =    
    (put n q |> getQ) = q


(*
// do Check.Quick p_ge
 
this is false but cannot be checked because we need to generate a queue q
     and the type is hidden. Options

     1.  Serialize (as we did for intset):  if we serialize say using
fromList, this is based on put and hence testing is not orthogonal

    2. Export the generator. Can be done, but the API writer may not wish to do that

Instead, we (the client) give an *abstract* notion of the operation over the ADT queue
and generate those.

Another instance of abstract interpretation
*)


type cmd =
    | E // empty
    | P of int // push: we fix ints, as we need some data type to generate with
    | G // getc

// now we simulate the effect of a series of symbolic command on a queue:
    // by writing a symbolic interpreter which take a Queue and returns
    // the result of applying the symbolic commands
   
//  interp : cs:cmd list -> Queue<int>

let rec interp cs =
        match cs with
            [] -> empty
            | E :: rest -> interp rest // skip and go on
            | P n :: rest ->  interp  rest |> put n // put n and go on
            | G :: rest -> let nq = (interp  rest) // do a get and go on
                           let (_,qq) = get nq
                           qq

(* Now we ask FsCheck to generate a list of symbolic commands
 -- no problems there -- that we interpret to get an effect on the queue

p_ges  sq: cmd list -> n:int -> bool
   *)
let p_ges sq (n : int) =
    try
        let q = interp sq
        printfn "gen queue is: %A" q
        (put n q |> getQ) = q
    with
        |_ -> true // if empty queue

do Check.Quick p_ges

// better test, removing empty queues

let p_gesb (n : int) =
        Prop.forAll (Arb.filter ( List.isEmpty >> not) Arb.from<cmd list>)
        <| fun sq ->
            try
                let q = interp  sq
                printfn "instructions: %A" sq
                (put n q |> getQ) = q
            with
                |_ -> true // if empty queue

do Check.Quick p_gesb            


(* the property is false: take the queue


 1 3

 and do a put 2

 1 3 2

 now a get will return  1 and 3 2, which is different from 2  and 1 3



   *) 


// We may want  to generate more meaningful sequences of instructions
let p_gesbf (n : int) =
    // a biased generator for commands
    let k = Gen.choose(-n,n)
    let genC = Gen.frequency [(1,gen {return E}); (5, Gen.map P k); (3, gen {return G})]
    // lift to cmd list and remove empty lists
    let genCs = (Gen.nonEmptyListOf genC)
    Prop.forAll ( Arb.fromGen genCs)
   <| fun sq ->
       try
           let q = interp  sq
           printfn "instructions: %A" sq
           (put n q |> getQ) = q
       with
           |_ -> true // if empty queue

(*

 *)
