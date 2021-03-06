﻿module Program

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
    let i, j = time Problems31to40.problem36
    printfn "%A, %.3fms" i j
    Console.ReadKey(true) |> ignore
    0