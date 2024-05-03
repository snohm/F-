// Computation expressions: book, chapter 12

// We're going to redo the option monad from scratch as a way to
// introduce the notion of computation expressions in general

// Motivation: recall our fav type checking example:
// it's boring to thread type errors or more in general partiality

type exp =
  | I of int                 
  | V of string           
  | Sum of exp * exp  
  | B of bool              
  | Eq of exp * exp    

// abbreviations
let (++) m n = Sum(m,n);;
let (==) m n = Eq(m,n);;

// expressions
let  et  = I 2 == (I 3 ++ V "x" ++  V "a");;
let et2 =    (V "x" ++  V "a") == (I 2);;
let et3 =    (V "x" ++  V "a") == (B true);;

// types and tenv
type tp = INT | BOOL;;
type tenviroment = Map<string,tp>;;

let atenv = Map.add "x" INT (Map.add "a" INT Map.empty);;

// the usual type checker returning options: 
//  tpchk : e:exp -> tenv:tenviroment -> tp option

let rec tpchk e (tenv : tenviroment) = 
    match e with
     V s -> Map.tryFind s tenv
    | I _ -> Some INT
    | B _ -> Some BOOL
    | Sum(e1,e2) ->
        match (tpchk e1 tenv, tpchk e2 tenv) with
            | Some t1, Some t2 when t1 = INT && t2 = INT  -> Some t1   
            | _ -> None
    | Eq(e1,e2)  ->
        match (tpchk e1 tenv, tpchk e2 tenv) with 
        | Some t1, Some t2 when t1 = t2 -> Some BOOL
        | _ -> None

(* The threading of options is annoying, makes the code complex and
does not scale if we do some serious railroad programming.

Computation expressions/workflows/monads are a way of wiring in this
plumbing.

In reality, monads are good at dealing with all sort of effects, which
makes a lot of sense in pure languages such as Haskell and Coq. For F#
this is more a question of elegance rather than necessity

Here we will mostly look at partiality and we will touch upom continuations.
We will also see that random generation is a monad.

We start by building the partiality monad, and for pedagogical reasons 
we start from scratch rather than leveraging the built-in option type.
      *)
 
// Homage to Haskell
type 'a maybe =
    | Nothing
    | Just of 'a

// Now, redefine (lift) basic operations on this container
// map, return, apply, bind

// mapM: f:('a -> 'b) -> opt:'a maybe -> 'b maybe
let mapM f opt =
    match opt with
        | Nothing    -> Nothing
        | Just x -> Just (f x)


// lifting of functional application to the container

//  applyM : f:('a -> 'b) maybe -> x:'a maybe -> 'b maybe
let applyM f x =
    match f,x with
        | Just f, Just x -> Just (f x)
        |  _ -> Nothing
        
//   return: x:'a -> 'a maybe                   
let returnM x = Just x                    

// bind is essentially a "let" in the container and the most important building block
// bindM m (fun x -> e) ~~ let x = m in e
        
// val bindM : m:'a maybe -> f:('a -> 'b maybe) -> 'b maybe
let bindM  m f =
    match  m with
        | Nothing   ->  Nothing
        | Just a -> f a

let (>>=) = bindM

// helper function so that we can use the Map collection in the monadic type checker
let opt2maybe = function
    None -> Nothing
    | Some z -> Just z

// We re-write the type-checker with maybe, but more importantly using
// "bindM" to sequence the checking of subexpression, so that 
// some of the partiality threading in a pattern matching is hidden. 

// val tpchkB : e:exp -> tenv:tenviroment -> tp maybe
let rec tpchkB e (tenv : tenviroment) = 
    match e with
     V s -> Map.tryFind s tenv |> opt2maybe
    | I _  -> returnM INT
    | B _  -> returnM BOOL
    | Sum(e1,e2) ->
        (* was:
        match (tpchk e1 tenv, tpchk e2 tenv) with
            | Some t1, Some t2 when t1 = INT && t2 = INT  -> Some t1   
            | _ -> None*)
        tpchkB e1 tenv >>= (fun t1 ->
                             tpchkB e2 tenv >>= (fun t2 -> 
            if  t1 = INT && t2 = INT  then  returnM t1 else  Nothing))
            (* i.e.: bindM ( tpchkB e1 tenv) (fun t1 -> (bindM  (tpchkB e2 tenv)  (fun t2 -> etc ))) *)
    | Eq(e1,e2)  ->
                tpchkB e1 tenv >>= (fun t1 ->
                             tpchkB e2 tenv >>= (fun t2 -> 
            if  t1 = t2   then  returnM BOOL else  Nothing))  

(* So far, we have just used some syntactic sugar. Now we introduce
the computation expression construction for turning it into a monad.

The syntax comes from C#'s classes. It's basically a type (class) augmented with
    some virtual methods ("bld" can be written as "this" if you prefer).

Which methods? You must have an implementation of Bind and Return, at
least. The others give a nicer syntax, are derived and will
be introduced as we go.  *)

type MaybeClass() =
    member bld.Bind(m, f) = bindM m f
    member bld.Return x  = returnM x
    member bld.Zero() = Nothing // for failure/if-then, 
    
// construct the "maybe" monad
let maybe = MaybeClass();;

// you'll find people using "new"
// let maybe = new maybeClass();;

// Nor rewrite (for the third time!) the type checker using "maybe" as
// a sort of constructor, much as you've been using "seq"

//tcm : exp ->  tenviroment -> maybe<tp>
let rec tcm e (tenv : tenviroment) = 
    match e with
    I _ -> maybe { return INT} // was: returnM INT
    | B _ -> maybe { return BOOL}
    | V s ->  Map.tryFind s tenv |> opt2maybe
    | Sum(e1,e2) -> maybe {  // here it pays off: note the monadic let
                    let! t1 = tcm e1 tenv
                    let! t2 = tcm e2 tenv
                    if t1 = INT &&  t2 = INT then return INT}
  // note: if-then w/o else, the None case is handled by the Zero method
    | Eq(e1,e2)  -> maybe{
                    let! t1 = tcm e1 tenv
                    let! t2 = tcm e2 tenv
                    if t1 = t2  then return BOOL}


let q3 = tcm et atenv 
let q4 = tcm et2 atenv
let q5 = tcm et3 atenv

(*
      let! x = e
      ce

      is sugar for Bind(e,fun x -> ce)

      if b then ce

       is sugar for

       if b then ce else  Zero()

                              *)

(* There are, of course, many other usages of the maybe monad ... *)

let div m n =
    maybe {if n <> 0 then return (m / n)}
// old version
let rec factOpt n =
        match n with
          | 0 -> Some 1                       // fact 0 = 1 
          | _  -> if n < 0 then None         // se n < 0, fact n  non e' definito 
                  else
                   match factOpt (n-1) with     // se n>  0, fact n = n * fact (n-1) 
                   |  Some k -> Some ( n * k)
                   | _  -> None
// monadic version
let rec factM n =
    maybe {
        match n with
        | 0 -> return 1
        | _ -> 
            if n > 0 then
                let! k = factM (n-1)
                return (n * k)
        }  
// can be written with pm and then the maybe        
let rec factM' n =
        match n with
        | 0 -> maybe { return 1}
        | _ -> maybe{
            if n > 0 then
                let! k = factM'  (n-1)
                return (n * k)
        }                             
type figura = 
        | Rettangolo of  float * float      // (base, altezza)
        | Quadrato   of  float              // lato
        | Triangolo  of  float * float      // (base, altezza)
     
let area fig =
   match fig with
   | Rettangolo(b,h) ->   b * h     
   | Quadrato l      ->   l * l  
   | Triangolo(b,h)  ->  ( b * h )  / 2.0 

let areaOpt fig =
    match fig with
        | Rettangolo (b,h) | Triangolo(b,h)  
             -> maybe {if b > 0. && h > 0. then return (area fig)}
        | Quadrato lato    ->  maybe {if lato >= 0. then return (area fig)}
    

let sommaArea (fig1, fig2) =
    maybe {
        let! a1 = areaOpt fig1
        let! a2 = areaOpt fig2
        return (a1 + a2)}

(*The function findM takes three parameters:
   - x, the first key to look up in the map
   - y, the second key to look up in the map
   - m, the map to look up in
The function returns a Maybe int if the lookup is successful *)
let findM x y (m: Map<'a,int>) =
    maybe {
        let! z1 = Map.tryFind x m |> opt2maybe
        let! z2 = Map.tryFind y m |> opt2maybe
        return (z1 + z2)
    }

// see tauto.fsx  for more details -- SKIP now
type p = T | A of p * p | O of p * p | F    

let rec tautom (f:p) =
    match f with
        | T -> maybe {return true}
        | F -> maybe {return false}
        | A(f1,f2) -> maybe {
            let! b1 = tautom f1
            let! b2 = tautom f2
            return (b1 && b2)}
        | O(f1,f2) -> maybe {
            let! b1 = tautom f1
            if not b1 then 
                let! b2 = tautom f2
                return (b2)}



