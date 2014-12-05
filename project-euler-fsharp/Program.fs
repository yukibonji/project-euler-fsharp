open System

let problem1a () =
    seq { for n in 1 .. 999 do if n % 3 = 0 || n % 5 = 0 then yield n } |> Seq.sum

let problem1b () =
    seq { for n in 1 .. 999 do if n % 3 = 0 || n % 5 = 0 then yield n } |> Seq.fold (fun running next -> running + next) 0

let problem2 () =
    let rec fibEvenSum fstFib sndFib runningTotal =
        let newFib = fstFib + sndFib

        if newFib % 2 = 0 then runningTotal := !runningTotal + newFib

        match newFib with
        | x when x > 4000000 -> runningTotal
        | _ -> fibEvenSum sndFib newFib runningTotal

    ref 0 |> fibEvenSum 1 1 |> (!)

let problem3a () =
    let factorsOf (n: int64) =
       let root = n |> float |> sqrt |> int64
       seq { for x in 2L .. root do if n % x = 0L then yield x }

    let isPrime n =
        factorsOf n |> Seq.length = 0

    let input = 600851475143L
    let factorsOfInput = factorsOf input

    match factorsOfInput |> Seq.length with
    | 0 -> input
    | _ -> factorsOfInput |> Seq.filter isPrime |> Seq.max 

let problem3b () =
    let input = 600851475143L

    let mutable n = 2L
    let mutable roof = input
    let mutable largestFactor = 1L

    while (n <= roof) do
        match roof % n with
        | 0L -> roof <- roof / n; largestFactor <- n
        | _ -> n <- n + 1L 

    largestFactor

let problem4 () =
    let isPalindrome tuple =
        let str = fst tuple * snd tuple |> string
        str = new string(Seq.toArray str |> Array.rev)

    seq { for i in 100 .. 999 do for j in 100 .. i do yield i, j } 
    |> Seq.filter isPalindrome
    |> Seq.map (fun (i, j) -> i * j)
    |> Seq.max

let problem5 () =
    let divisors = [ 2L; 3L; 4L; 5L; 6L; 7L; 8L; 9L; 11L; 13L; 16L; 17L; 19L ]

    let rec gcd a b =
        match a % b with
        | 0L -> b
        | n -> gcd b n

    divisors |> Seq.fold (fun cur next -> (*) cur next |> (/) <| gcd cur next) 1L

let problem6a () =
    (seq { 1 .. 100 } |> Seq.sum |> pown) 2 |> (-) <| (seq { 1 .. 100 } |> Seq.map (fun i -> pown i 2) |> Seq.sum)

let problem6b () =
    let n = 100

    (pown (n * (n + 1) / 2) 2) - (2 * n + 1) * (n + 1) * n / 6

[<EntryPoint>]
let main argv = 
    problem6b() |> printfn "%d"

    Console.ReadKey(true) |> ignore
    0