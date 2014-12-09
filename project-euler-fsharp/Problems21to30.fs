module Problems21to30

open System
open Common

let problem21 () =
    let factorsSum = seq { 0L .. 9999L }
                     |> Seq.map (factorsOf >> Seq.sum >> (+) 1L >> int)
                     |> Seq.toArray
    let len = Array.length factorsSum

    let isAmicable i =
        let value = factorsSum.[i]
        value < len && value <> i && factorsSum.[value] = i
    
    seq { 0 .. 9999 }
    |> Seq.filter isAmicable
    |> Seq.sum