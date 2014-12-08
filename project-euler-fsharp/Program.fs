module Program

open System
open System.Diagnostics

let time f =
    let sw = new Stopwatch()
    sw.Start()

    let result = f ()

    sw.Stop()

    result, sw.ElapsedTicks |> double |> (/) <| double Stopwatch.Frequency

[<EntryPoint>]
let main argv = 
    let i, j = time Problems11to20.problem18
    printfn "%d, %.3fms" i <| j
    Console.ReadKey(true) |> ignore
    0