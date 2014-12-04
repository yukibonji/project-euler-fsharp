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

let problem3a () =
    let factorsOf (n: int64) =
       let root = n |> float |> sqrt |> int64
       seq { for x in 2L .. root do if n % x = 0L then yield x }

    let isPrime n =
        factorsOf n |> Seq.length = 0

    let input = 600851475143L
    let factorsOfInput = factorsOf input

    if factorsOfInput |> Seq.length = 0 then
        input
    else
        factorsOfInput |> Seq.filter isPrime |> Seq.max

let problem3b () =
    let input = 600851475143L

    let mutable n = 2L
    let mutable roof = input
    let mutable largestFactor = 1L

    while (n <= roof) do
        if roof % n = 0L then
            roof <- roof / n
            largestFactor <- n
        else
            n <- n + 1L

    largestFactor

[<EntryPoint>]
let main argv = 
    problem3b () |> printfn "%d"

    Console.ReadKey(true) |> ignore
    0