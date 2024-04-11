(*let rec sumSeq sq index= 
    let index = index + 1 
    let subsq = List.take index sq
    //let subsq = Seq.toList subsq
    let ris = List.foldBack (fun x acc -> x + acc ) subsq 0 
    match (index) with
    | 11 -> [(ris)]
    | _ -> (ris) :: sumSeq sq index
sumSeq [0..10] 0
*)

let nat = Seq.initInfinite id
let rec sumSeq sq index = 
    let index = index + 1 
    let subsq = Seq.take index sq
    let subsq = Seq.toList subsq
    let ris = List.foldBack (fun x acc -> x + acc ) subsq 0 
    seq{
       yield ris 
       yield! sumSeq sq index
    }
Seq.toList(Seq.take 10 (sumSeq nat 0));;
