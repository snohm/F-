// a first naive encodings: finite sets as lists w/o repetitions.
// Non having repetitions is the INVARIANT: functions (e.g. add)
// *assume* the input list is a set and must *guarantee* that so is the result.


// same name of sig file
module IFSet

// the type implementation: it **must** be a tagged type (or a record). 
// The more if we use a type abbreviation: how can the compiler hide it otherwise?

type IFSet = S of int list;;

(*
- we **must** give implementation for all the declarared functions

- we *can* use local functions
*)

let empty =  S [];;

let isEmpty (S ss)  = List.isEmpty ss;;

// O(|ss|)
let contains x (S ss) =
    List.exists (fun y ->  x = y) ss;;

let add x ss = 
        if contains x ss then ss else
          let (S xs) = ss
          S (x :: xs)

// same but with pm in the head
let add1 x (S ss) = 
        if contains x (S ss) then (S ss) else S (x :: ss)

// 𝑂(𝑛) ∗𝑂(𝑛) = 𝑂(𝑛^2)
let unionslow (S s1) s2 = List.foldBack add s1 s2

// tail recursive, but reverses the list, which is OK because we do not care about order
let union (S s1) s2 =
    List.fold (fun  xs x  ->  add x xs) s2 s1

let toList (S ss) = ss;;

// we need to remove duplicates
let ofList ss = union (S ss) empty;;

// breaking the invariant

let wrong = S ([1;1]);;







