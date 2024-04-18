#r  "FsCheck"
open FsCheck

type tp = INT | LSTINT

type tm =
  K of int
  | Plus of tm * tm
  | Nil
  | Cons of tm * tm
  | Hd of tm
  | Tl of tm

// tpck : _arg1:tm -> tp option
let rec tpck = function
  | K _ -> Some INT
  | Nil -> Some LSTINT
  | Plus(e1,e2) ->
    match(tpck e1,tpck e2) with
      (Some(INT),Some(INT)) -> Some(INT)
      | _ -> None
  | Cons(e1,e2) ->
    match(tpck e1,tpck e2) with
      (Some(INT),Some(LSTINT)) -> Some(LSTINT)
      | _ -> None
  | Hd e ->
    match(tpck e) with
      Some(LSTINT) -> Some(INT)
      | _ -> None
  | Tl e ->
    match(tpck e) with
      Some(LSTINT) -> Some(LSTINT)
      |_ -> None
  


let test size len =
  let tms = (Gen.sample size len Arb.generate<tm>)
  List.map2 (fun x y -> printf "%A has type: %A\n" x y) tms ( List.map tpck tms)

let _ = test 20 10

// 2. // rewrite so to catch None  
let testb size len =
  let tms = (Gen.sample size len Arb.generate<tm>)
  List.map (fun x  ->
            match (tpck x) with
            | None -> printf "%A has no type \n" x
            | Some t -> printf "%A has type %A\n" x t) tms 

let _ = testb 10 100

// 6
let rec eval = function
  K n -> K n
  | Nil -> Nil
  | Plus(e1,e2) -> let (K n1) = eval e1
                   let (K n2) = eval e2
                   K(n1 + n2)
  | Cons(e1,e2) -> Cons(eval e1, eval e2)
  | Hd e -> let (Cons (vh, _))  = eval e in vh
  | Tl e -> let (Cons ( _, vt)) = eval e in vt


let rec evalo = function
  | K n -> Some (K n)
  | Nil -> Some Nil
  | Plus(e1,e2) ->
    match(evalo e1,evalo e2) with
      | Some (K(n1)), Some( K(n2)) -> Some (K(n1 + n2))
      | _ -> None
  | Cons(e1,e2) ->
        match(evalo e1,evalo e2) with
          | Some h, Some tl -> Some (Cons(h,tl))
          | _ -> None
  | Hd e ->
    match evalo e with
      | Some (Cons (vh, _))  -> Some vh
      | _ -> None
  | Tl e ->
    match evalo e with
      | Some (Cons (_, vt))  -> Some vt
      | _ -> None

// 5
let rec value = function
  K _ | Nil  ->  true
  |Cons(e1,e2) -> value e1 && value  e2
  |  _ -> false


let prop_vs e =
    let genwt = Arb.mapFilter id (fun x -> tpck x <> None) Arb.from<tm>
    let res = evalo e
    Prop.forAll genwt (fun e ->
                     match res with
                       |None -> true
                       | Some v -> value v)
    |> Prop.trivial (Option.isNone res)

do Check.Quick prop_vs



let eok = eval (Hd (Cons(Plus(K 3,K 9),Nil)))
let etk = eval(Tl (Cons(K 3,Nil)))

// 3.1
exception NotINT of (tm * tp) * (tm * tp)
exception NotLST of (tm * tp) * (tm * tp)
exception HdERR of (tm * tp) 
exception TlERR of (tm * tp) 

// 3.2
let rec tpckf = function
  | K _ ->  INT
  | Nil ->  LSTINT
  // | Plus(e1,e2) ->  let (t1,t2) = (tpckf e1, tpckf e2)
  //                   if t1 = INT && t2 = INT 
  //                       then t1 
  //                           else raise  (NotINT ((e1,t1),(e2, t2)))

  | Plus(e1,e2) ->
      match (tpckf e1, tpckf e2) with
          INT, INT -> INT
          |_ -> raise  (NotINT ((e1,INT),(e2, INT)))


  | Cons(e1,e2) ->
      match (tpckf e1, tpckf e2) with
          INT, LSTINT -> LSTINT
          |_ -> raise  (NotINT ((e1,INT),(e2, LSTINT)))

// keep here the let
  | Hd e ->
              let t = tpckf e
              if t = LSTINT then INT else raise (HdERR(e,t))
  | Tl e -> let t = tpckf e
            if t = LSTINT then LSTINT else raise (TlERR(e,t))


// 4
let ht e =
  try
     printf "%A has type %A\n" e (tpckf e) 
  with
    | NotINT((e1,t1),(e2,t2)) ->  printf "%A does not type check. " e ; printf "Expected types %A : INT, %A : INT. Inferred types: %A and %A\n" e1 e2 (e1,t1) (e2,t2)
    | NotLST((e1,t1),(e2,t2)) -> printf "%A does not type check. " e ; printf "Expected types %A : INT, %A : LSTINT. Inferred types: %A and %A\n" e1  e2 (e1,t1) (e2,t2)
    | HdERR(e1,t2) -> printf "%A does not type check. " e ; printf "Expected type: INT. Inferred type: %A\n " (e1,t2)
    | TlERR(e1,t2) -> printf "%A does not type check. " e ; printf "Expected type: INT. Inferred type: %A\n " (e1,t2) 
    

let f size len =  [for e in (Gen.sample size len Arb.generate<tm>) do  ht e] |> ignore

do f 20 10

let wellTypedTermsOf =
        let isWellTyped e = tpck e |> Option.isSome
        Arb.mapFilter (fun x -> x) isWellTyped Arb.from<tm>


let prop_pres =
  Prop.forAll wellTypedTermsOf <| fun e ->
    let t =  tpck e  // need to recheck the type, so t is a Some _
    let v = evalo e
    match v with
      | None -> true
      | Some v -> t = tpck v
    |> Prop.trivial (Option.isNone v)    


let config = { Config.Quick with MaxTest = 1000; StartSize = 10; EndSize = 20}
do Check.One(config,prop_pres)


             
