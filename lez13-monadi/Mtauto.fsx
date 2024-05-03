(* We write a tautology checker for a simple language using
options to implement backtracking -- checking could be done
using builtin || (of if-then-else), but we use it as a gentler introduction
ro what comes next.
*)

type p = TT | AND of p * p | OR of p * p | FF

// a pretty printer
let rec prettyp f  =
    match f with
        | TT -> "true"
        | FF -> "false"
        | AND(p1,p2) -> sprintf "(%s /\ %s)" (prettyp p1) (prettyp p2)
        | OR(p1,p2) -> sprintf "(%s \/ %s)" (prettyp p1) (prettyp p2)


(* Now we check for validity. Note how in the disjunction case we check the first
disjunct and only if it is false do we check the second.
*)
let rec tautoo (f:p) : bool option =
    match f with
        |TT -> Some true
        | AND(p1,p2) ->
            match tautoo p1, tautoo p2 with
            | Some b1, Some b2 -> Some (b1 && b2)
            | _ -> None
        | OR(p1,p2) ->
            match tautoo  p1 with
            | Some true -> Some true // if true, that's it, otherwise we check the second
            | _ -> tautoo  p2
        | FF -> None // here we fail and backtrack

#r "FsCheck.dll"
open FsCheck

// let's run some examples
let sample() =
    match (Gen.sample 20 1 Arb.generate<p>) with
        [form] ->
        match (tautoo form) with
            None -> prettyp form + " is FALSE"
            | Some p1 -> prettyp form + " is TRUE"
        | _ -> "does not happen"


let s1 = sample()

// let's try to write a monadic version, this time using directly a
// maybe class out of options

type MaybeClass() =
    member bld.Bind(m, f) = Option.bind f m 
    member bld.Return x  = Some x
    member this.ReturnFrom m   = m  
    member bld.Zero() = None // for failure/if-then, 
    
// construct the "maybe" monad
let maybe = MaybeClass();;

// we ca also do the pm outside the monad
let rec tautom f =
    maybe{
        match f with
            | TT -> return true
            | FF -> return false
            | AND(f1,f2) -> 
                let! b1 = tautom f1 
                let! b2 = tautom f2 
                return (b1 && b2)
            | OR(f1,f2) -> 
                let! b1 = tautom f1
                if (not b1) then 
                    let! b2 = tautom f2
                    return b2
                    }
                    
let sample2() =
    match (Gen.sample 20 1 Arb.generate<p>) with
          [form] ->
             match (tautoo form) with
                None -> prettyp form + " is false"
                | Some p1 -> prettyp form + " is true"
        | _ -> "does not happen"


(* Again: this would've worked with || for OR and false for F, w/o the need of
 backtracking behaviour, but this is just motivation for next step,
 where we return a "proof" of the tautology. 
 
 A  *proof* can be a constant for True, say C, a pair of proofs for conjunction, and for disjunction
 an indication if the left or right disjunct is provable (an injection). No proof term
 for False, since, well, it's not provable.

Syntax for proofs: 
p ::= <> | <p1,p2> | left p | right p
*)

type disj = L | R
type proof = C | P of proof * proof | In of  disj * proof 

// now the function returns the evidence by which a formula is true
// first the non-monadic version tautop : form -> proof option

let rec tautoop (f:p)  =
        match f with
            |TT -> Some C 
            | AND(p1,p2) ->
                match tautoop p1, tautoop p2 with
                | Some b1, Some b2 -> Some (P (b1, b2))
                | _ -> None
            | OR(p1,p2) ->
                match tautoop   p1 with
                | Some b  -> Some (In(L,b))
                | _ -> match tautoop  p2 with
                        | Some b -> Some (In(R,b))
                        | _ -> None
            | FF  -> None

// simplified via monads
let rec tautoopM (f:p)  =
    maybe {
                match f with
                    |TT -> return C
                    | FF -> return! None 
                    | AND(p1,p2) ->
                        let! b1 = tautoopM p1
                        let! b2 = tautoopM p2
                        return (P (b1, b2))
                    | OR(p1,p2) ->
                    match tautoop p1 with
                    | Some b  -> return (In(L,b))
                    | _ -> let! b = tautoopM p2
                           return (In(R,b)) }

                        
                        

let sample3 () =
    match (Gen.sample 20 1 Arb.generate<p>) with
          [form] ->
             match (tautoop  form) with
                None -> prettyp form + " is FALSE"
                | Some p1 -> prettyp form + " is TRUE with proof: " + p1.ToString()
        | _ -> "does not happen"

// we can skip the next bit:
    
// We can recover the formula from its proof
// the inverse function is not really inverse since we discard info on
// the branch not taken and we replace it with F

// check : proof -> form
let rec check = function
    | C -> TT 
    | P(p1,p2) -> AND(check p1, check p2)
    | In(L,p)  -> OR(check p,FF)
    | In(R,p) -> OR(FF,check p)
                                