module Program

open System
open System.Diagnostics

let time f =
    let sw = new Stopwatch()
    sw.Start()

    let result = f ()

    sw.Stop()

    result, sw.ElapsedTicks |> float |> (/) <| float TimeSpan.TicksPerMillisecond

[<EntryPoint>]
let main argv = 
    let i, j = time Problems11to20.problem17
    printfn "%d, %.3fms" i <| j
    Console.ReadKey(true) |> ignore
    0