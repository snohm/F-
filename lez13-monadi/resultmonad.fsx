        (* ... But here we switch to another monad to refine the type checking example.  In
fact, we can improve the type checker with the Result monad, so as to
differentiate between failure reasons.  *)

// We use the builtin type Result and its combinators

// The monad builder
type ResultBuilder() =
    member this.Bind(x,f) = Result.bind f x
    member this.Return x = Ok x
    member this.ReturnFrom(m : Result<'a,'b>)  = m // for return!
    member bld.Zero() = Error "Type error" 
        // for if-then, to provide a generic type error
    

let result = ResultBuilder()

let opt2result err = function
    None -> Error err
    | Some z -> Ok z


type exp =
        | I of int                 
        | V of string           
        | Sum of exp * exp  
        | B of bool              
        | Eq of exp * exp    
type tp = INT | BOOL;;
type tenviroment = Map<string,tp>;;      
// once again, the type checker v 4.0
let rec tcR e (tenv : tenviroment) = 
    match e with
    I _ -> result { return INT}
    | B _ -> result { return BOOL}
    | V s -> result { return! 
                            Map.tryFind s tenv 
                            |> opt2result 
                                (sprintf "var %s not found in %A " s tenv)}
    | Sum(e1,e2) -> result {
                    let! t1 = tcR e1 tenv
                    let! t2 = tcR e2 tenv
                    // if t1 = INT &&  t2 = INT then return INT 
                    // here we use Zero                    
                    if t1 = INT &&  t2 = INT then return INT 
                    else return! (Error <| sprintf "type %A or type %A not an INT" t1 t2)} 
    | Eq(e1,e2)  -> result{
                    let! t1 = tcR e1 tenv
                    let! t2 = tcR e2 tenv
                    if t1 = t2  then return BOOL
                    else return! (Error <| 
                        sprintf "type %A not equal to type %A" t1 t2)}  // here we write our own
                        
#r "nuget:FsCheck"
open FsCheck

(* A monad is more then just a computation builder in so far it has to
obey the 3 monadic laws. F# -- as Haskell, and differently from Coq and
F* - does not allow us to encapsulate axioms in code, so they are a
sort of afterthought:

Here we formulate the right identity one as a property that we can
test:

(rid):  (n >>= return) = n
                              *)
/// The right identity monad law as a property of Result

let rightIdentity_prop (n : Result<int,string>) =
    let m1 = result{ let! x = n 
                     return x }
    let m2 = result { return! n }
    m1 =  m2

do Check.Quick rightIdentity_prop

// For the record, the other axioms

// return a	>>= 	h	≡ 	h a

(*
ce { x′ <- return x;
     f x′
   }
   ==
       ce { f x }
                      *)
let leftIdentity x f = 
  let m1 = result{ let! v = result{ return x }
               return! f v } 
  let m2 = f x
  m1 = m2


/// The associativity monad law 
/// (m >>= g) >>= h
// ==
// m >>= (\x -> g x >>= h)

let associativity n f g =
  let m1 = result{ let! y = result{ let! x = n
                            return! f x }
               return! g y }
  let m2 = result{ let! x = n
               return! result{ let! y = f x
                           return! g x } }
  let m3 = result {
      let! x = n
      let! y = f x
      return! g x }                        

  m1 = m2 && m2 = m3
 
// what about other containers? Lists? See next file
