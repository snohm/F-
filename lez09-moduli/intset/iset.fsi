// interface for finite  set of integers

module IFSet

// the type specification:

type IFSet

(*
no implementation: type is kept abstract

Here the spec of the functions (the interface) -- a subset of the Set library
*)

val empty : IFSet 

val isEmpty : IFSet -> bool 

val contains  : int ->  IFSet -> bool 

val add : int -> IFSet -> IFSet 

val union : IFSet -> IFSet -> IFSet 

val ofList :  int list -> IFSet 

val toList :  IFSet  -> int list 
