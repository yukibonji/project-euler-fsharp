module Problems11to20

open System
open System.Collections.Generic
open Common

let problem11 () =
    let inputMatrix = readLines 11
                      |> Seq.map (fun line -> line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse)
                      |> Seq.toArray
   
    let iter = [ 0 .. 3 ]
    let maxProductFromIndex (matrix: int [][]) i j =
        seq {
            if i < 17 then yield iter |> List.fold (fun state k -> state * matrix.[i + k].[j]) 1
            if j < 17 then yield iter |> List.fold (fun state k -> state * matrix.[i].[j + k]) 1
            if i < 17 && j < 17 then yield iter |> List.fold (fun state k -> state * matrix.[i + k].[j + k]) 1
            if j < 17 && i >= 3 then yield iter |> List.fold (fun state k -> state * matrix.[i - k].[j + k]) 1
            yield 0
        }
        |> Seq.max
    
    seq { for i in 0 .. 19 do for j in 0 .. 19 do if inputMatrix.[i].[j] <> 0 then yield maxProductFromIndex inputMatrix i j }
    |> Seq.max

let problem12 () =
    Seq.initInfinite id
    |> Seq.scan (+) 0
    |> Seq.pick (fun i -> if int64 i |> factorsOf |> Seq.length > 498 then Some(i) else None)

let problem13 () =
    // 11 may not always be enough
    let significantNumbers = readLines 13
                             |> Seq.map (fun str -> str.Substring(0, 11) |> Int64.Parse)

    (significantNumbers |> Seq.sum |> string).Substring(0, 10) |> Int64.Parse

let problem14a () =
    let mutable longest = 1, 0L

    for i in 2L .. 1000000L do
        let mutable l = 1
        let mutable curr = i

        while curr <> 1L do
            match curr with
            | Even -> curr <- curr / 2L
            | Odd -> curr <- 3L * curr + 1L
            l <- l + 1

        if l > fst longest then longest <- l, i

    snd longest

let problem14b () =
    let collatzLength n =
        let rec tailCollatzLength n acc =
            match n with
            | 1L -> acc
            | x when x % 2L = 0L -> tailCollatzLength (n / 2L) (acc + 1)
            | _ -> tailCollatzLength (3L * n + 1L) (acc + 1)

        tailCollatzLength n 1

    seq { 2L .. 999999L }
    |> Seq.maxBy collatzLength

let problem15 () =
    seq { 1L .. 20L }
    |> Seq.fold (fun state i -> state * (41L - i) / i) 1L

let problem16 () =
    2I ** 1000 |> string |> Seq.sumBy (fun i -> int i - 48)

let problem17 () =
    let numberLengths = new Dictionary<int, int>()

    // 0 has a length of -3 to get rid of 'and' in, say, '300', which is parsed as 'three hundred and'
    [| (0, -3); (1, 3); (2, 3); (3, 5); (4, 4); (5, 4); (6, 3); (7, 5); (8, 5); (9, 4); (10, 3); 
       (11, 6); (12, 6); (13, 8); (14, 8); (15, 7); (16, 7); (17, 9); (18, 8); (19, 8); (20, 6);
       (30, 6); (40, 5); (50, 5); (60, 5); (70, 7); (80, 6); (90, 6); (100, 7); (1000, 8) |]
    |> Array.iter numberLengths.Add

    let rec letterCount n =
        match n with
        | x when x < 21 -> numberLengths.[n]
        | x when x < 100 -> numberLengths.[n / 10 * 10] + if n % 10 <> 0 then numberLengths.[n % 10] else 0
        | x when x = 1000 -> numberLengths.[1000] + numberLengths.[1]
        | _ -> letterCount (n % 100) + numberLengths.[100] + 3 + numberLengths.[n / 100]

    seq { 1 .. 1000 }
    |> Seq.sumBy letterCount

let problem18 () =
    let inputTree = readLines 18
                    |> Seq.map (fun line -> line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse)
                    |> Seq.toArray
    let treeDepth = Array.length inputTree

    seq { treeDepth - 2 .. -1 .. 0 }
    |> Seq.iter (fun i -> seq { 0 .. i }
                          |> Seq.iter (fun j -> inputTree.[i].[j] <- inputTree.[i].[j] + max inputTree.[i + 1].[j] inputTree.[i + 1].[j + 1]))

    inputTree.[0].[0]