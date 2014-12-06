module Program

open System

[<EntryPoint>]
let main argv = 
    Problems1to10.problem10b () |> printfn "%d"
    Console.ReadKey(true) |> ignore
    0