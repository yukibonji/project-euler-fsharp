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

let problem22 () =
    let raw = readLines 22 |> Seq.nth 0

    raw.Split(',')
    |> Seq.map (fun s -> s.Substring(1, s.Length - 2))
    |> Seq.sort
    |> Seq.fold (fun (sum, i) s -> s |> Seq.sumBy (fun c -> int c - 64) |> (*) i |> (+) sum, i + 1) (0, 1)
    |> fst