// TYPING

(**  RIPASSO: ESPRESSIONI  INTERE  CONTENENTI VARIABILI     **)

(*


- Semantica dei linguaggi di programmazione
 -- definiamo un modello minimale di linguaggio di programmazione 
  --- sintassi
  --- semantica operazionale (interprete)
  --- semantica statica (sistema di type checking)
  --- proprietà auspicate di validità: type preservation

^^^^^^^^^^^^^^^^^^^^

Una espressione intera di tipo expv e' definita da

   e  ::=  x | n |  e1 + e2 

*)

// scegliamo un tipo per le variabili


type vname = string

type expv =
  | V of vname          
  | C of int
  | Sum of expv * expv

(*
Valutazione di expv (interprete "definizionale")

John C. Reynolds. Definitional interpreters for higher-order
programming languages. In Proc. ACM ’72: Proceedings of the ACM Annual
Conference, 1972.

*)


// Per rappresentare un ambiente usiamo la collezione delle funzioni finite

type envt = Map<vname,int> ;;

let env1 = Map.add "x" 55 Map.empty;;
let env2 = Map.add "x" 1 env1;;
let env3 = Map.add "y" 2 env2;;
let v = Map.find "y" env3 
let no = Map.tryFind "pippo" env3 


(*

Il giudizio

  env |- e >> v 

indica che l'espressione e (espressione di tipo expv) ha valore v (un intero) nell'ambiente env.
   
Definiamo  'env |- e >>  v' per induzione sulla struttura dell'espressione e.

Questo si scrive in letteratura con *regole*, dove un  *assioma*
 è una regole senza premesse che "chiude" la derivazione


 ---------------------- (eC)                ------------------------------ (eV)
  env |- C n >> n                       env |-  V x >> env(x)


 env |- e1 >> n1      env |- e2 >> n2
 --------------------------------------------------- (eS)
  env |- Sum(e1,e2) >> n1 + n2                             



==> Esempio.

Consideriamo l'ambiente 

  envxyz = { (x,1) , (y,2) , (z,3) }

Allora:
                                    envxyz |> V z >> 3     envxyz |> C 10 >> 10
------------------------          -------------------------------------------------------------
envxyz |>  V y >> 2        envxyz |> Sum( V z, C 10) >> 3 + 10
--------------------------------------------------------------------------------------------
  envxyz |-  Sum (V y, (Sum (V z) , (C 10))  >> 2 + (3 + 10) = 15


*)

// interprete per il linguaggio aritmetico, che implementa le regole precedenti
// evalv : expv -> envt -> int  

let rec evalv e ( env : envt)  =
    match e with
    | V x ->  Map.find x env  // calcola env(x)  -- lancia eccezione  se x not in dom(env)
    | C n -> n
    | Sum(e1,e2)   ->
        let n1 = evalv e1 env
        let n2 = evalv e2 env
        n1 + n2

(*
Nota la relazione tra interprete e descrizione a regole:

 evalv e env = v   sse   env  |-  e >> v
 
*)

// Esempi

// envxyz = { (x,1) , (y,2) , (z,3) }    
let envxyz = Map.ofList [ ("x",1) ; ("y",2) ; ("z", 3)] ;;         
let vv = evalv  (Sum(Sum ( V "y" , V "z" ),  C 10)) envxyz;; // 15 




// FINE RIPASSO

// Towards typing: expressions ranging over integers and booleans

type exp =
  | I of int                 (* integers *)
  | V of vname               (* vars *)
  | Sum of exp * exp         (* addition *)
  | B of bool                (* true/false         *)
  | Eq of exp * exp          (* equality     *)
  | Neg of  exp              (* negation     *)
  | Less of exp * exp;;      (* less *)


// infix notation
let (++) m n = Sum(m,n);;
let (==) m n = Eq(m,n);;


type value =
    | VI of int
    | VB of bool        
// for enviroments we use again the class Map, but map vars to exp, not to int

type enviroment = Map<vname,value>;;

let anenv = Map.add "x" (VI 3) (Map.add "a" (VI -7) Map.empty);;

// 2 + (3 + x + a)
let  et  = I 2 ++ (I 3 ++ V "x" ++  V "a");;

(*

 evaluation
                  eval : e:exp -> env:enviroment -> value
                                                                        ^^^^^
                                                                       
Note that differently from before we do not return an integer/bool,
but an value, because a well typed function must choose what to return. 
*)

let rec eval e env =
    match e with
    | I n      ->  VI n
    | B b     ->  VB b
    | V s      -> Map.find s env
    | Sum(e1,e2) -> 
        let (VI n1) = eval e1 env
        let (VI n2) = eval e2 env
        VI (n1 + n2)
    | Eq(n,m) -> 
        let v1 = eval n env
        let v2 = eval m env
        VB (v1 = v2) 
    | Less(n,m) ->  
        let (VI v1) = eval n env
        let (VI v2) = eval m env
        VB (v1 < v2) 
    | Neg b ->  
        let (VB nb) = eval b env
        VB (not nb);;   


let p1 = eval et anenv;;

(*

 | Sum(e1,e2) -> let (VI n1) = eval e1 env
  ------------------------^^^^^^

 warning FS0025: Incomplete pattern matches on this expression. For
 example, the value 'VB (_)' may indicate a case not covered by the
 pattern(s).

What's going on here?

Incomplete pattern match signals our wishful thinking: we "know" that
eval should return an integer, but nothing is stopping us from doing
something silly such as  summing two bools *)

let stupid =
    try
        eval (Sum (B true, B false)) Map.empty |> string
    with
        |_ -> "bad idea"


(*
This (implicitly) raises a Microsoft.FSharp.Core.MatchFailureException, (because
the F# compiler implicitely adds raise MatchFailureException to non
exhaustive patterns)

Another source of problem: env may **not** contain the value of a certain key:
(in imperative terms, variable is NOT initialized)
  
*****************************************

We rewrite the interpreter to make it "defensive", so that it explicitly raises
exceptions if called on the wrong arguments (or the variable is
undefined).

First we introduce some exceptions (we do not handle them yet, though).
*)

exception NotABool;;
exception NotAInt;;
exception NotSameType;;
exception UndefVar of vname;;

///  defensive evaluation
let rec evald e env =
    match e with
    | I n     ->  VI n
    | B b     ->  VB b
    | V s     -> 
        try
            Map.find s env 
        with
            | :? System.Collections.Generic.KeyNotFoundException -> raise (UndefVar s)  // _ -> raise (UndefVar s) 
  
    | Sum(e1,e2) -> 
        match(evald e1 env, evald e2 env) with // any evaluation of a subexpression can raise exn
           | (VI n1,VI n2) -> VI (n1 + n2)
           | (_,_) -> raise NotAInt 
    | Eq(n,m) -> 
        match(evald n env, evald m env) with
            | (VB b1,VB b2)  -> VB (b1 = b2)
            | (VI n1,VI n2)  -> VB (n1 = n2)
            | (_,_) -> raise NotSameType  
    | Less(e1,e2)  -> 
        match(evald e1 env, evald e2 env) with
            | (VI n1,VI n2) -> VB (n1 < n2)
            | (_,_) -> raise NotAInt 
    | Neg b ->  
        match(evald b env) with
            (VB nb) -> VB (not nb)
            | _ -> raise NotABool;; 

let less_stupid = evald (Sum (B true, B false)) Map.empty;;

(*
***********************************************
OK, now we get no warnings and we could handle the exceptions
with a top level main function

    Happy puppy?

Not quite

This is called "dynamic typing" and basically is what happens in
languages such as Perl, Python, Prolog, Lisp etc.

This is bad because

1. It is inefficient: we keep checking (class) tags at RUN-TIME 
2. It makes the code complicated: here just two tags (two types), but think about real languages
3. It's pointless: these checks can be done at COMPILE-TIME

Hence, we will do this as TYPE-CHECKING

        THE PHASE DISTINCTION (Luca Cardelli 1988).

        compile vs run-time

Aside: it's true that in this toy language we could avoid run time
checks separating boolean expressions from arithmetic ones, or we could
interpret booleans as ints, but this is a general point


Why bother with types?

 ==> Because they prevent mistakes.

 They are a simple, automatic way to find obvious problems in programs
before these programs are ever run.

There are 3 kinds of types.

* The Good: Static types that guarantee absence of certain runtime faults.

* The Bad: Static types that have mostly decorative value but do not guaran-
tee anything at runtime.

* The Ugly: Dynamic types that detect errors only when it can be too late.

Examples of the first kind are Java, F# and Haskell. In Java for instance,
the type system enforces that there will be no memory access errors, which in
other languages manifest as segmentation faults. F# and Haskell have even
more powerful type systems that can be used to enforce basic higher-level
program properties by type alone, for instance strict information hiding in
modules or abstract data types.

Famous examples of the bad kind are C and C++. These languages have
static type systems, but they can be circumvented easily. 

Ugly: Examples for dynamic types are scripting languages such as Perl and
Python. These languages are monotyped, but typing violations are
 discovered and reported at runtime only, which leads to runtime
messages such as “TypeError: . . . ”

The ideal for a static type system is to be permissive enough not to
get into the programmer’s way while being strong enough to achieve
Robin Milner’s slogan

          "Well-typed programs cannot go wrong"

 What could go wrong? Some examples of common runtime errors are:
 
- corruption of data,
- null pointer exceptions,
- nontermination,
- running out of memory,
- leaking secrets...

There exist type systems for all of these, and more, but in practise
only the first is covered in widely-used languages such as Java, C#,
Haskell etc


For a very good discussion of static vs dynamic typing, see

http://blog.steveklabnik.com/posts/2010-07-17-what-to-know-before-debating-type-systems
*)

type tp = INT | BOOL;;

type tenviroment = Map<vname,tp>;;

(* 

Judgment:
                tenv |- e : t, where (_) is the rule name


------------------------- (var)
 tenv |- x : tenv x


------------------------ (int)
 tenv |- C n : INT


---------------------- (bool)
 tenv |- B b : BOOL


 tenv |- e1 : INT      tenv |- e2 : INT
 ------------------------------------------- (sum)
 tenv |- Sum(e1,e2) : INT


 tenv |- e1 : INT      tenv |- e2 : INT
 -------------------------------------------- (less)
 tenv |- Less(e1,e2) : BOOL


 tenv |- e1 : t  tenv |- e2 : t
 ----------------------------------- (eq)
 tenv |- Eq(e1,e2) : BOOL


 tenv |- e : BOOL
 ---------------------------- (not)
 tenv |- Not e : BOOL


  tnvxyz = { (x,INT) , (y,INT) , (z,INT) }

                                            ------------------------- (var)  --------------------------- (int)
                                             tnvxyz |> V z : INT          tnvxyz |> C 10 : INT
------------------------ (var)         ------------------------------------------------------------- (sum)
tnvxyz |>  V y : INT        tnvxyz |> Sum( V z, C 10) : INT
-------------------------------------------------------------------------------- (sum)
  tnvxyz |-  Sum (V y, (Sum (V z) , (C 10))  : INT

*)

// Note: a FP encoding of rules has to make explicit the *partiality* of the judgment
// This will be strikingly different in relational languages

// first with options to handle partiality

let rec tpchko e (tenv : tenviroment)  = 
    match e with
     V s -> Map.tryFind s tenv
    | I(_) -> Some INT
    | B(_) -> Some BOOL
    | Sum(e1,e2)   ->
        match (tpchko e1 tenv, tpchko e2 tenv) with
            Some INT, Some INT -> Some INT
            | _ ->  None
                            
    | Eq(e1,e2)  -> 
        match (tpchko e1 tenv, tpchko e2 tenv) with
            Some t1, Some t2 when t1 = t2 -> Some BOOL
            | __-> None

    | Less(e1,e2)  ->  
        match (tpchko e1 tenv, tpchko e2 tenv) with
            Some INT, Some INT -> Some BOOL
            | _ ->  None 
    | Neg b -> 
        match tpchko b tenv with
            Some BOOL -> Some BOOL
            | _ -> None



// more informative to give meaningful errors with Result, with
// construccors Ok and Error
            

let rec tpchkR e (tenv : tenviroment)  = 
    match e with
     V s ->
         match (Map.tryFind s tenv) with
             | Some t -> Ok t
             |  None ->  sprintf  "Variable %s NOT in %A" s tenv |> Error
    | I(_) -> Ok INT
    | B(_) -> Ok BOOL
    | Sum(e1,e2)   ->
        match (tpchkR e1 tenv, tpchkR e2 tenv) with
            Ok INT, Ok INT -> Ok INT
            | t1,t2 -> sprintf "%A and/or %A not an int" t1 t2 |> Error
                            
    | Eq(e1,e2)  -> 
        match (tpchkR e1 tenv, tpchkR e2 tenv) with
            Ok t1, Ok t2 when t1 = t2 -> Ok BOOL
            |   t1 , t2 -> sprintf "%A : %A, but %A : %A" e1 t1 e2 t2 |> Error

    | Less(e1,e2)  ->  
        match (tpchkR e1 tenv, tpchkR e2 tenv) with
            Ok INT, Ok INT -> Ok BOOL
            | t1,t2 -> sprintf "%A : %A, but %A : %A" e1 t1 e2 t2 |> Error
    | Neg b -> 
        match tpchkR b tenv with
            Ok BOOL -> Ok BOOL
            |   t -> sprintf "%A not a boolean" t |> Error


// examples
let atenv = Map.add "x" INT (Map.add "a" INT Map.empty);;
let p3 = tpchkR et atenv;; 
let t4 = tpchko et atenv;; 


let et2 =    (V "x" ++  V "a") == (I 2);;
let p4 = tpchkR et2 atenv;; 

let et3 =    (V "x" ++  V "a") == (B true);;


let notty = tpchkR et3 atenv





// Let's put the two together: evaluating only well typed programs, aka a simulation of the REPL
// tc_ev : e:exp -> tenv:tenviroment -> env:enviroment -> unit

let main  e tenv env =
    match tpchkR e tenv with
        | Ok t ->      sprintf "%A has type: %A and value: %A\n"  e t (eval e env)
        | Error s ->  "TYPE ERROR: "+ s

let ok1 = main  et2 atenv anenv;; 
let notok1 = main et3 atenv anenv;; 


// some metatheory

(* How do we know that types and interpreters go together? We should prove
the fundamental preservation property:

if e : t and e >> v, then v : t

This guarantees soundness of the type system. Note that it applies here to **closed** terms.

In this course, we do not do this kind of proofs (but see later the
lecture on type-preserving interpreters).

If you're interested in these topics, we talk about that in the Metodi
Formali course

... but we can do some testing -- see defining PL in _language workbench_ such as

-- PLT-Redex

-- Spoofax

-- K-framework

*)

#r "nuget:FsCheck"
open FsCheck

// To do testing over meta-theory we need to get a little deeper wrt PBT.

// we can change the default setting of 100 tests, using FsCheck config record
// here we do 500 and fiddle with the size

let config = { Config.Quick with MaxTest = 500; StartSize = 10; EndSize = 100}

let tpval = function
    VI _ -> INT
    | VB _ -> BOOL

let prop_pres_naive e =
      let t = tpchko e Map.empty
      (t <> None ==>  lazy (Option.get t =  (eval e Map.empty |> tpval )))
      

do Check.One(config,prop_pres_naive)

// as expected, coverage is an issue, but let's look into this

// size of a term
let rec cnt = function
    I _ | V _ |B _ -> 1
    | Sum(e1,e2) | Eq(e1,e2) | Less(e1,e2) -> 1 + cnt e1 + cnt e2
    | Neg e -> 1 + cnt  e

// we can monitor the distribution with the function  Prop.collect :  ('a -> 'b -> Property)
// here we break down the generation according to size


let prop_pres_naive_c e =
    let t = tpchko e Map.empty
    (t <> None ==>  lazy (Option.get t =  (eval e Map.empty |> tpval )))
    |> Prop.collect (cnt e)

do Check.One(config,prop_pres_naive_c)

// Very small expressions, so not a great test. We can do a bit better
// by writing our own generator of well typed terms

// This takes the default generator for exp Arb.from<exp> and keeps
// only the well typed terms

let wellTypedTermsOf =
    Arb.filter (fun x -> tpchko x Map.empty <> None) Arb.from<exp>

// Still not great: here a sample

let a() =
    wellTypedTermsOf
    |> Arb.toGen
    |> Gen.sample 100 20
    

//  However, we can restate the preservation property quantifying only
// on well typed terms with the combinatori Prop.forAll. Note that
//    forall (e:G). P  ~~ forall G (fun e -> P)


let prop_pres =
    Prop.forAll wellTypedTermsOf // forall e: tau, ...
     (fun e ->
      let v = eval e Map.empty
      let t = tpchko e Map.empty
      (t <> None ==>  lazy (Option.get t =  tpval v))
      )

do  Check.One(config,prop_pres)

// So, coverage is better, however ...
   
let prop_pres_c =
    Prop.forAll wellTypedTermsOf // forall e: tau, ...
     (fun e ->
      let v = eval e Map.empty
      let t = tpchko e Map.empty
      (t <> None ==>  lazy (Option.get t =  tpval v))
      |> Prop.collect (cnt e)
      )
    
do  Check.One(config,prop_pres_c)


// Still, very small terms. To do better, we should write our own
// generator from scratch. We'll see how in a later lecture (maybe).

(*
open FsCheck

let rec expGen (size: int) (tenv: tenviroment) : Gen<exp> =
    let leafGen =
        Gen.oneof [
            Gen.map I Arb.generate<int>
            Gen.map B Arb.generate<bool>
            Gen.map V (Gen.elements (Map.keys tenv))
        ]

    let rec expGenAux s =
        match s with
        | 0 -> leafGen
        | n when n > 0 ->
            let subexpGen = expGenAux (n / 2)
            Gen.oneof [
                leafGen
                Gen.map2 (fun e1 e2 ->
                    match tpchko e1 tenv, tpchko e2 tenv with
                    | Some INT, Some INT -> Sum(e1, e2)
                    | _ -> Gen.elements [e1; e2]
                ) subexpGen subexpGen
                Gen.map2 (fun e1 e2 ->
                    match tpchko e1 tenv, tpchko e2 tenv with
                    | Some INT, Some INT -> Less(e1, e2)
                    | Some t1, Some t2 when t1 = t2 -> Eq(e1, e2)
                    | _ -> Gen.elements [e1; e2]
                ) subexpGen subexpGen
                Gen.map (fun e ->
                    match tpchko e tenv with
                    | Some BOOL -> Neg e
                    | _ -> e
                ) (expGenAux (n / 2))
            ]
        | _ -> invalidArg "s" "Only positive arguments are allowed."

    expGenAux size

let tenv = Map.ofList [("x", INT); ("y", BOOL)]
let wellTypedExpGen = expGen 5 tenv

open FsCheck
*)

let rec expGen (size: int) (tenv: tenviroment) : Gen<exp> =
    let leafGen =
        Gen.oneof [
            Gen.map I Arb.generate<int>
            Gen.map B Arb.generate<bool>
            Gen.map V (Gen.elements (Map.keys tenv))
        ]

    let rec expGenAux s =
        match s with
        | 0 -> leafGen
        | n when n > 0 ->
            let subexpGen = expGenAux (n / 2)
            Gen.oneof [
                leafGen
                Gen.map2 (fun e1 e2 ->
                    match tpchko e1 tenv, tpchko e2 tenv with
                    | Some INT, Some INT -> Sum(e1, e2)
                    | _ -> Eq(e1, e2)
                ) subexpGen subexpGen
                Gen.map2 (fun e1 e2 ->
                    match tpchko e1 tenv, tpchko e2 tenv with
                    | Some INT, Some INT -> Less(e1, e2)
                    | _ -> Eq(e1, e2)
                ) subexpGen subexpGen
                Gen.map (fun e ->
                    match tpchko e tenv with
                    | Some BOOL -> Neg e
                    | _ -> e
                ) (expGenAux (n / 2))
            ]
        | _ -> invalidArg "s" "Only positive arguments are allowed."

    expGenAux size

let tenv = Map.ofList [("x", INT); ("y", BOOL)]
let wellTypedExpGen = expGen 5 tenv
