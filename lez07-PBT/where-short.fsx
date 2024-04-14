#r "nuget:FsCheck";;
open FsCheck
open System


// PBT PATTERNS: slides first!

(* ======================================================
Different paths, same destination
====================================================== *)

let add x  y = x + y

// First a weak spec: note that we pass the sorting function as an argument


let ``+1 then sort should be same as sort then +1`` sortFn xs = 
    
    let result1 = xs |> sortFn |> List.map (add 1)
    let result2 = xs |> List.map (add 1) |> sortFn 
    result1 = result2

// good  test    
do Check.Quick (``+1 then sort should be same as sort then +1``  List.sort)
// bad test
do Check.Quick (``+1 then sort should be same as sort then +1``  id)

    
// a spec which actually uses the sortedness
let ``append minValue then sort should be same as sort then prepend minValue`` sortFn xs = 
    let minValue = Int32.MinValue
   
    let appendThenSort = (xs @ [minValue]) |> sortFn 
    let sortThenPrepend = minValue :: (sortFn xs)
    appendThenSort = sortThenPrepend 

// test
Check.Quick (``append minValue then sort should be same as sort then prepend minValue`` List.sort)

Check.Quick (``append minValue then sort should be same as sort then prepend minValue`` id)
// bad implementation fails

(* ======================================================
"There and back again"
====================================================== *)

(* The first example often mentioned is the "hello world!" of PBT:

rev (rev xs) =   xs.

Note that's not typical as reverse is its own inverse. It's more an
involution, see afterwards

*)

// an example with char (fu)
let ``upper and lower a char is a char``  c =
   ((Char.ToUpper c |> Char.ToLower) = c)

do Check.Quick ``upper and lower a char is a char`` 

// obviously if a char is already upper, lowering won't get you back to the original
let ``lowering an upper char is not the same``  c =
   (Char.IsUpper c) ==> (Char.ToLower c <> c)

do Check.Quick ``lowering an upper char is not the same``   

// back to the prevoius example
let ``upper and lower a char is a char2``  c =
  ((Char.ToUpper c |> Char.ToLower) = c) || ((Char.ToLower c |> Char.ToUpper) = c)

do Check.Quick ``upper and lower a char is a char2`` 




/// Converts a list of characters into a string.
let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()


/// To explode a string into a list of characters, 
/// we see a string as a sequence of char
let ``explode and implode are inverse``  cs =
  implode cs |> Seq.toList = cs 

do Check.Quick  ``explode and implode are inverse``

let ``implode and explode are inverse``  s =
   Seq.toList s |> implode = s 

do Check.Quick ``implode and explode are inverse``


// add/contains

let ``if I  add to a set, it's there`` (x: int) ss  =
  (Set.union (Set.singleton x) ss) |>  Set.contains x

do Check.Quick  ``if I  add to a set, it's there``;;



(* ======================================================
"Some things never change"
====================================================== *)

let ``sorting preserves elements`` (xs: int list)=
  set xs = set (List.sort xs)

do Check.Quick  ``sorting preserves elements``

let ``map preserves len`` f (xs: int list) =
  List.length xs =  (List.map f xs |> List.length)

// here FSCheck invents a function
do Check.Quick    ``map preserves len``

// here I pass one myself
do Check.Quick    (``map preserves len`` abs)


//======================================================
//The more things change, the more they stay the same"
//====================================================== 

let ``sort idempotent`` (xs : int list) =
  List.sort (List.sort xs) = List.sort xs

do Check.Quick  ``sort idempotent``

let  ``toupper idempotent`` c =
  (Char.ToUpper c |> Char.ToUpper) = (Char.ToUpper c);;

do Check.Quick  ``toupper idempotent``


