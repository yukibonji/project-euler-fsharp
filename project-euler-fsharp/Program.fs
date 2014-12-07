module Program

open System
open System.Diagnostics

let time f =
    let sw = new Stopwatch()
    sw.Start()

    let result = f ()

    sw.Stop()

    result, sw.ElapsedMilliseconds

[<EntryPoint>]
let main argv = 
    let i, j = time Problems11to20.problem14b
    printfn "%d, %dms" i <| int j
    Console.ReadKey(true) |> ignore
    0