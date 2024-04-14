
module Poly
type poly

val ofList : int list -> poly
val toList :  poly ->  int list
val notNullPoly: poly -> bool
val multxc : poly -> int -> poly
val multxx : poly ->  poly
val opposto : poly -> poly
val somma : poly -> poly -> poly
val diff :  poly -> poly -> poly
val grado : poly -> int 
