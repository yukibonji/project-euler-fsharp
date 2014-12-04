open System

let problem1a () =
    seq { for n in 1 .. 999 do if n % 3 = 0 || n % 5 = 0 then yield n } |> Seq.sum

let problem1b () =
    seq { for n in 1 .. 999 do if n % 3 = 0 || n % 5 = 0 then yield n } |> Seq.fold (fun running next -> running + next) 0

let problem2 () =
    let rec fibEvenSum fstFib sndFib runningTotal =
        let newFib = fstFib + sndFib

        if newFib % 2 = 0 then runningTotal := !runningTotal + newFib

        if newFib > 4000000 then
            runningTotal
        else
            fibEvenSum sndFib newFib runningTotal

    ref 0 |> fibEvenSum 1 1 |> (!)

[<EntryPoint>]
let main argv = 
    problem2 () |> printfn "%d"

    Console.ReadKey(true) |> ignore
    0