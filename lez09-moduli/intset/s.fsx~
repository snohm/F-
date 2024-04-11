// using the dll

//#r "listFS.dll"
#r "bin/Debug/net6.0/list1.dll"
open IFSet

// let's create a set

let ss = add 3 empty |> add 5 |> add 2 |> add 77 


// what? I can see the representation type!
// Yes, that's unfortunate (it's unclear why), but you *CANNOT* manipulate it
// just try S/Leaf;;


// this are really sets:
let ss1 = (add 3 ss) ;;

let oneset = ofList [1..10]


let u = union oneset ss ;;


// let's test our Set implementation wrt the Set collection
#r "nuget:FsCheck"
open FsCheck

(*
 - a suite of tests, as a list, each with a label to identify the culprit

- note that use of List as a mediation betweeb FSet and Set

 + we cannot generate FSet directly, since the representatio type is "hidden"

- note that use of List.sort to avoid false positives due to FSet as list  not being ordered

*)


let test_set x (xs : int list) ys =
  [
    empty |> toList = (Set.empty |> Set.toList) |@ "empty set"
    isEmpty (ofList xs) = Set.isEmpty (Set.ofList xs) |@ "is empty"
    contains x (ofList xs) = Set.contains x (Set.ofList xs) |@ "contains"
    (add x (ofList xs) |> toList |> List.sort) = (Set.add x (Set.ofList xs) |> Set.toList)  |@ "add"
    (union  (ofList xs) (ofList ys) |> toList |> List.sort) =
       (Set.union  (Set.ofList xs)  (Set.ofList ys)|> Set.toList) |@ "union"
    (ofList xs |> toList |> List.sort ) = (Set.ofList xs |> Set.toList) |@ "list"
    ]

do Check.Quick test_set


(* Let's actually use this ADT for something useful: evaluation of
logical formulae, where propositional variables takes values in an
enviroment 'env', with the understanding that if a variable occurs in env, then
it's true, otherwise false. Example

V 2 true in [1..10]

V 22 false in [1..10]
*)

type form =
  V of int
  | K of bool
  | Not of form
  | And of form * form


type env = IFSet

let rec eval env = function
  K b -> b
  | V i -> contains i env
  | Not f -> eval env f |> not
  | And (f1, f2) -> (eval  env f1) && (eval env f2)


let test() =
    let env = Gen.sample 20 20 Arb.generate<int> |> Set.ofList |> Set.toList
    // generate an env with at most 20 elements, and sort it removing dups
    match Gen.sample 30 1 Arb.generate<form> with
        // generate a formula with size at most 30
        [e] ->
            let res =  eval ( IFSet.ofList  env) e
            printfn "\tinput formula: %A\n\tinput env: %A\n\tresult of evaluation: %A" e env res
        |_ -> failwith "does not happen"

// we'll see how to define better generators in a bonus lecture
// for one, need to correlate the vars in the formula and the enviroment        
    
  
