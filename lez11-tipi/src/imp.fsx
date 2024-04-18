// bonus file: static and dynamic semantics of IMP

// Recall the the exp language
type vname = string

type exp =
  | I of int                 (* integers *)
  | V of vname               (* vars *)
  | Sum of exp * exp         (* addition *)
  | Diff of exp * exp         (* difference *)
  | Prod of exp * exp         (* mult *)
  | B of bool                (* true/false         *)
  | Eq of exp * exp          (* equality     *)
  | Neg of  exp              (* negation     *)
  | Less of exp * exp;;      (* less *)

// syntax of IMP
type cmd  =                    (* statements             *)
   | Ass of string * exp       (* assignment             *)
   | Skip                      (* empty st               *)
   | Seq  of cmd * cmd         (* sequential composition *)
   | ITE   of exp * cmd * cmd  (* if-then-else           *)
   | While of exp * cmd;;      (* while                  *)

// y:=1 ; while not(x=0) do (y:= y*x ; x:=x-1)
// factorial with x free (to be set in the env)
let fac = Seq(Ass("y", I 1),
              While(Neg(Eq(V "x", I 0)),
                    Seq(Ass("y", Prod(V "x", V "y")) ,
                        Ass("x", Diff(V "x", I 1)) )));;

type enviroment = Map<vname,exp>;;

// usual evaluation of exp
let rec eval e env =
    match e with
    | I n      ->  I n
    | B b     ->  B b
    | V s      -> Map.find s env
    | Sum(e1,e2) -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 + n2)
    | Diff(e1,e2) -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 - n2) 
    | Prod(e1,e2) -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 * n2)
    | Eq(n,m) -> 
        let v1 = eval n env
        let v2 = eval m env
        B (v1 = v2) 
    | Less(n,m) ->  
        let (I v1) = eval n env
        let (I v2) = eval m env
        B (v1 < v2) 
    | Neg b ->  
        let (B nb) = eval b env
        B (not nb);;   


// infix notation
let (++) m n = Sum(m,n);;
let (%%) m n = Diff(m,n);;
let (==) m n = Eq(m,n);;

// interpreters for IMP (non-defensive)
let rec exec cmd env =
    match cmd with
    | Ass(x,a)         ->
        let v = eval a env
        Map.add  x v env
    | Skip             -> env
    | Seq(cmd1, cmd2)  ->
        let env' = exec cmd1 env
        exec cmd2 env'
    | ITE(b,cmd1,cmd2) ->
        let (B test) = eval  b env
        if test then exec cmd1 env else exec cmd2 env
    | While(b, cmd)    ->
        let (B test) = eval   b env
        if test then
            let env' = exec cmd env
            exec (While(b, cmd)) env'
                                   else env

// IF 2 = 2 THEN SKIP else x := 4
let ifexp1 = ITE(Eq(I 2, I 2),Skip,Ass("x", I 4));;
let p5 = exec ifexp1 Map.empty;;

// IF 2 * (3 + 2) = 10 THEN SKIP else x := 4
let ifexp2 = ITE(I 2 %%  (I 3 ++ I 2) == I 1,Skip,Ass("x", I 4));;
let p6 = exec ifexp2 Map.empty;;

// factorial
let s0 = Map.ofList [("x",(I 4))];;

let ff = exec fac s0;;


type tp = INT | BOOL;;

type tenviroment = Map<vname,tp>;;

// type checking exp
let rec tpchko e (tenv : tenviroment)  = 
    match e with
     V s -> Map.tryFind s tenv
    | I(_) -> Some INT
    | B(_) -> Some BOOL
    | Sum(e1,e2)  | Prod(e1,e2)  | Diff(e1,e2)  ->
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

// type checking IMP programs; impOK : cmd:cmd -> tenv:tenviroment -> bool


(*
sample rule: 

tenv |- x : t	tenv |- a : ta   ta = t
----------------------------------------------------
    tenv |- x := a OK

*)

// should improve error messages
let rec impOK cmd tenv =
    match cmd with
        | Ass(x,a) -> 
                  match (Map.tryFind x tenv) with
                      | None -> failwithf "Variable %s NOT in %A" x tenv  
                      |Some t -> 
                      match (tpchko a tenv) with
                          None -> failwith "not typable " 
                          | Some ta -> if ta = t then true else  failwith "not typable" 

        | Skip             -> true
        | Seq(cmd1, cmd2)  -> (impOK cmd1 tenv) && (impOK cmd2 tenv)
        | ITE(b,cmd1,cmd2) ->
            if (tpchko b tenv) = (Some BOOL)
                            then (impOK cmd1 tenv) && (impOK cmd2 tenv) 
                               else failwith "type error in test"
        | While(b, cmd)    ->
            if (tpchko b tenv) = (Some BOOL )
                            then (impOK cmd tenv) 
                              else failwith "type error in test"
                                     


let t0 = Map.ofList [("x", INT);("y", INT)];;

let fff = impOK fac t0;;

