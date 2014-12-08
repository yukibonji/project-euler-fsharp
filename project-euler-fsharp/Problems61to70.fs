module Problems61to70

open System
open Common

let problem67 () =
    let inputTree = readLines 67
                    |> Seq.map (fun line -> line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse)
                    |> Seq.toArray
    let treeDepth = Array.length inputTree

    seq { treeDepth - 2 .. -1 .. 0 }
    |> Seq.iter (fun i -> seq { 0 .. i }
                          |> Seq.iter (fun j -> inputTree.[i].[j] <- inputTree.[i].[j] + max inputTree.[i + 1].[j] inputTree.[i + 1].[j + 1]))

    inputTree.[0].[0]