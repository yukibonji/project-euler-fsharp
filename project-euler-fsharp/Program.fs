module Program

open System
open System.Diagnostics

let time f =
    let sw = new Stopwatch()
    sw.Start()

    let result = f ()

    sw.Stop()

    result, sw.Elapsed.TotalMilliseconds

[<EntryPoint>]
let main argv = 
    let i, j = time Problems21to30.problem26
    printfn "%d, %.3fms" i <| j
    Console.ReadKey(true) |> ignore
    0