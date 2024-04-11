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
let rec fact n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * fact (n-1)
let rec pow x k =
    match k with 
    | 0 -> 1.
    | _ -> x * pow x (k - 1)

let f x k =
    let num = pow x k
    num / float(fact k)
let rec sumSeq sq index = 
    let index = index + 1 
    let subsq = Seq.take index sq
    let subsq = Seq.toList subsq
    let ris = List.foldBack (fun x acc -> x + acc ) subsq 0 
    seq{
       yield ris 
       yield! sumSeq sq index
    }
let apprTaylor x = 
   
    seq{

    }