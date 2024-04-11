// how to model-test ADT symbolically

#r "nuget:FsCheck"
open FsCheck
open System.Collections.Generic

// Imperative queues from .NET (realized as circular arrays)
// basic interface

// we refer to the names used in the Queue.fsi interface

//  val empty: Queue<int>
let empty = new Queue<int>()

// val top: q: Queue<'T> -> 'T
let top (q : Queue<'T>) =
    q.Peek()  // this just return head, w/o changing the queue

// val put: n: 'T -> q: Queue<'T> -> unit
let put n  (q : Queue<'T>) =
    q.Enqueue n

// val pop: q: Queue<'T> -> 'T
// instead of getting head of queue and rest, we only get head    
let pop   (q : Queue<'T>) = q.Dequeue()




// sample session

// start with empty queue    
let q = empty ;;

try 
    top q
with 
 | _ -> -1

put 1 q;;

put 2 q;;

put 3 q

pop q;;

top q;;

q.Clear();; // clear the memory

q.Count

// since the data type is abstract (and from .NET) we cannot test it,
// as no generation is allowed.

// uncomment
// do Check.Quick (fun (q : Queue<int>) -> q = q)



(*
What can we do?

- If we had a deserializer such as fromList, we could generate list
elements and then convert them to queues, but we don't

Idea: test imperative queues against a (functional) **model** by abstracting the queue operation into
abstract versions that can be generated. It requires these steps:


1. Define a type of abstract commands and the (mathematical) model ("state")
         --> here: queues as lists, get is List.head, put is append

2. Wite an interpreter, which is akin to a transition system: given a command
and a state, returns a new state  -- [function next_state]

3. Execute commands on the implementation [function run_cmd]

    - if the command does an obervations, it must be the same in the model and the implementation

    - if the command is side-effecting, do it and go on

4. Extend this to a set of commands: s = cs = q    [function  interp_agree ]

     -----------
    s = [] = q


    run_cmd s c q           s -- c --> s'           s' = cs = q
    ------------------------------------------------------------------
                          s = (c :: cs) = q


5. Write a test (and possibly a biased generator) to check that
the above relation holds for a random set of commands

    *)

// Our model  of queues are naive lists

type state = int list

// Model level commands
type cmd =
    | Top // peek
    | Put of int // put
    | Pop // get the top


(* The command interpreter

next_state : val next_state: c: cmd -> s: state -> state

simulate one step of execution of an abstract command over a state

--> think transitions in a state machine

- A push command alters the state by inserting a new element
last, at the right.

- Since top will not alter the internal state, it just returns the
state s unmodified.

-- pop needs a non-empty queue state to remove from, so in case the
state is empty [] the model remains unmodified.  *)

// 
let next_state c s =
    match c with
        | Pop ->
            match s with
             | []    -> []
             | _::s' -> s'
        | Top -> s
        | Put i -> s@[i]


// its kleene closure (just for debugging purposes)
let rec next_state_start s = function
    [] -> s
    | c :: cs ->
        let s' = next_state c s
        next_state_start s' cs

(*
 Now we write an interpreter that
establishes the equivalence between abstract and concrete operations

 Note; operations are partial over empty queues

*)

exception EmptyQueue

let pre_cond s (q: Queue<int>) =
    q.Count > 0
    &&
    s|> (List.isEmpty >> not)

(*

run_cmd: c: cmd -> s: state -> q: Queue<int> -> bool


 Since put commands have no return value, we simply perform
the corresponding queue operation and return true to signal
success. Both pop and top perform the corresponding queue command and
compare the result with the front of the list.  Either of pop , top ,
or List.hd can raise an exception,  and this is why we have a pre condition:*)

let run_cmd c s q = 
    match c with
    | Pop ->
        if pre_cond s q then  pop q = List.head s else raise EmptyQueue
    | Top ->
        if pre_cond s q then  top q = List.head s else raise EmptyQueue
    | Put n -> put n q; true


(* a recursive agreement checker that walks
the list of symbolic commands: *)

//  s: state -> q: Queue<int> -> cs: cmd list -> bool
let rec interp_agree s q cs =
    match cs with
        | [] -> true
        | c::cs ->
            let b = run_cmd c s q 
            let s' = next_state c s 
            b && interp_agree s' q cs

(* Now we have no problem for FsCheck to generate a list of symbolic commands
 that we interpret to get an effect on the queue
   *)

// agreement between implementation and model

let agree_test cs =
    let q = new Queue<int>() // this must be done here
    try
        interp_agree [] q cs
    with
        | _ -> true // if we raise an exception, we don't care
do Check.Quick agree_test


// biased generator for more meaningful sequences of instructions
let genAQ n =
    let k = Gen.choose(-n,n)
    Gen.frequency [(3,gen {return Top}); (6, Gen.map Put k); (2, gen {return Pop})] |> Gen.nonEmptyListOf

let  b_agree_test (n : int) =
    let q = new Queue<int>()
    Prop.forAll ( Arb.fromGen (genAQ n))
    <| fun cs ->
        (
            try
            interp_agree [] q cs
            with
            | _ -> true)

do Check.Quick b_agree_test


// END
// UNUSED
// variations of the main methods
let putQ n  (q : Queue<'T>) =
    let itm = q.Enqueue n
    q

let get  (queue: Queue<'T>) =
    let itm = queue.Dequeue()
    (itm, queue)

let getQ  (q : Queue<int>) =
    get q |> snd 

(*
let run_cmd c s q = 
    match c with
    | Pop ->
        try
            pop q = List.head s
        with _ -> false
    | Top ->
        try
            top q = List.head s
        with _ -> false
    | Put n -> put n q; true
          *)
