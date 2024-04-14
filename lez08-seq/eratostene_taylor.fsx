// setaccio di eratostene 
let rec nat1 x = seq{
    if x > 1 then yield x
    yield! nat1 (x + 1)
}
let nat = Seq.initInfinite id
let sift x sq = Seq.filter (fun y -> y % x <> 0 ) sq
sift 2 nat |> sift 3 |> sift 4 |>sift 5 |>sift 6 |>sift 7|>sift 8|>sift 9|>sift 10|>sift 11|>sift 12 |>Seq.take 50 |> Seq.toList

let rec sieve sq = 
    let h = Seq.head sq
    let tail = Seq.tail sq
    let ris = sift h tail 
    seq{
        yield! Seq.append (seq [h]) (sieve ris)
    }
let primes = sieve (nat1 0) |> Seq.take 1000 |> Seq.toList

// versione + veloce
let siftC a sq = Seq.cache  ( sift a sq )   
let rec sieveC sq = 
    let h = Seq.head sq
    let tail = Seq.tail sq
    let ris = siftC h tail 
    seq{
        yield! Seq.append (seq [h]) (sieveC ris)
    }
let primesC = sieveC (nat1 0) |> Seq.take 1000 |> Seq.toList

// APPROSIMAZIONE DELLA FUNZIONE ESPONENZIALE MEDIANTE SERIE DI TAYLOR

let f x k =
    let rec fact n =
        match n with
        | 0 | 1 -> 1
        | _ -> n * fact (n-1)
    let rec pow x k =
        match k with 
        | 0 -> 1.
        | _ -> x * pow x (k - 1)
    pow x k / float(fact k)
let sumSeq sq =
    let rec sums sq acc= 
        let h = Seq.head sq
        let sq = Seq.tail sq
        seq{
          yield h + acc 
          yield! sums sq (h + acc) 
        }
    sums sq 0 
let apprTaylor x = 
    let rec helper n acc=
        seq{
            yield (f x n) + acc
            yield! helper (n+1) ((f x n) + acc)  
        }
    helper 0 0
apprTaylor 1.0 |> Seq.take 10 |> Seq.toList
let apprExp x delta =
    let taylor = apprTaylor x
    let rec helper seqExp delta =
        let e0 = Seq.head seqExp
        let seqExp = Seq.tail seqExp
        let e1 = Seq.head seqExp
        let seqExp = Seq.tail seqExp
        if abs (e1 - e0) < delta then e0 else helper seqExp delta   
    helper taylor delta

//Esempi:

apprExp 1.0 0.01  
// val it : float = 2.708333333

apprExp 1.0 0.0001 
// val it : float = 2.718253968

apprExp 1.0 0.0000001 
// val it : float = 2.718281801

apprExp 2.5 0.0000001 
// val it : float = 12.18249394