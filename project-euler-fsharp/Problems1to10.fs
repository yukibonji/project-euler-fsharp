module Problems1to10

open System
open System.Linq
open Common

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
    let input = 600851475143L
    let factorsOfInput = factorsOf input

    match factorsOfInput |> Seq.isEmpty with
    | true -> input
    | _ -> factorsOfInput |> Seq.filter isPrime |> Seq.max 

let problem3b () =
    let input = 600851475143L

    let mutable n = 2L
    let mutable roof = input
    let mutable largestFactor = 1L

    while (n <= roof) do
        match roof with
        | Even -> roof <- roof / n; largestFactor <- n
        | Odd -> n <- n + 1L 

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

    divisors |> Seq.fold (fun cur next -> (*) cur next |> (/) <| gcd cur next) 1L

let problem6a () =
    (seq { 1 .. 100 } |> Seq.sum |> pown) 2 |> (-) <| (seq { 1 .. 100 } |> Seq.sumBy (fun i -> pown i 2))

let problem6b () =
    let n = 100
    (pown (n * (n + 1) / 2) 2) - (2 * n + 1) * (n + 1) * n / 6

let problem7 () =
    Seq.initInfinite (fun i -> int64 i |> (*) 2L |> (+) 1L)
    |> Seq.filter isPrime
    |> Seq.nth 10000

let problem8 () =
    let numberArrays = String.Join("", readLines 8).Split([| '0' |], StringSplitOptions.RemoveEmptyEntries)
                                                   .Where(fun inner -> String.length inner > 12)
                                                   .ToArray()
                                                   |> Array.map (fun inner -> inner.ToCharArray() |> Array.map (fun i -> int64 i - 48L))
    
    let productFromIndexes (array: int64 []) indexes =
        indexes |> List.fold (fun state i -> array.[i] * state) 1L

    numberArrays
    |> Array.map (fun i -> seq { 13 .. Array.length i - 1 }
                           |> Seq.scan (fun state j -> state / i.[j - 13] * i.[j]) (productFromIndexes i [ 0 .. 12 ])
                           |> Seq.max)
    |> Array.max

let problem9 () =
    let isPythTriplet tuple =
        let third = 1000 - fst tuple - snd tuple
        fst tuple * fst tuple + snd tuple * snd tuple = third * third

    seq { for i in 1 .. 333 do
            for j in 333 .. 500 do
              yield i, j }
    |> Seq.pick (fun (i, j) -> if isPythTriplet (i, j) then Some(i * j * (1000 - i - j)) else None)

let problem10a () =
    seq { 3L .. 2L .. 1999999L }
    |> Seq.fold (fun state i -> if isPrime i then i :: state else state) [ 2L ]
    |> List.sum

let problem10b () =
    let root = float 1999999 |> sqrt |> int64
    let mutable sieve = [ 3L .. 2L .. 1999999L ]

    for i in 3L .. root do
        sieve <- List.filter (fun x -> x = i || x % i <> 0L) sieve
    
    List.sum sieve + 2L