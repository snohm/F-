Aggiungere all'interfaccia iset.fsi le seguenti funzioni

count : IFSet -> int, che computa la cardinalità dell'insieme

map : (int -> int) -> IFSet -> IFSet, 
       dove map f s = {f(x) | x in s}

(tenendo presente di preservare l'invariante che il risultato sia un
insieme, cioè non abbia ripetizioni; per esempio applicare map (fun x
-> 42) ad un insieme di interi restuisce l'insieme che contiene solo
42)

isSubset : IFSet->IFSet-> bool, che controlla se un insieme è
sottoinsieme di un altro

min : IFSet-> int, che calcola il minimo elelmento di un insieme non vuoto

--> Implementare le dette funzioni sia come liste che come alberi.

--> Estendere i test fscheck dati a coprire le nuove funzionalità
