module Program

open System
open System.Linq

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
    let factorsOfInput = Common.factorsOf input

    match factorsOfInput |> Seq.length with
    | 0 -> input
    | _ -> factorsOfInput |> Seq.filter Common.isPrime |> Seq.max 

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

    divisors |> Seq.fold (fun cur next -> (*) cur next |> (/) <| Common.gcd cur next) 1L

let problem6a () =
    (seq { 1 .. 100 } |> Seq.sum |> pown) 2 |> (-) <| (seq { 1 .. 100 } |> Seq.map (fun i -> pown i 2) |> Seq.sum)

let problem6b () =
    let n = 100
    (pown (n * (n + 1) / 2) 2) - (2 * n + 1) * (n + 1) * n / 6

let problem7 () =
    Seq.initInfinite (fun i -> int64 i |> (*) 2L |> (+) 1L)
    |> Seq.filter Common.isPrime
    |> Seq.nth 10000

let problem8 () =
    let numberArrays = "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450".Replace(Environment.NewLine, "").Split([| '0' |], StringSplitOptions.RemoveEmptyEntries)
                                                   .Where(fun inner -> String.length inner > 12).ToArray()
                                                   |> Array.map (fun inner -> inner.ToCharArray() |> Array.map (fun i -> int64 i - 48L))
    
    let productFromIndexes (array: int64 []) indexes =
        indexes |> List.fold (fun state i -> array.[i] * state) 1L

    numberArrays
    |> Array.scan (fun _ i -> seq { 13 .. Array.length i - 1 }
                              |> Seq.scan (fun stateInner j -> stateInner / i.[j - 13] * i.[j]) (productFromIndexes i [ 0 .. 12 ])
                              |> Seq.max)
                  0L
    |> Array.max

[<EntryPoint>]
let main argv = 
    problem8() |> printfn "%d"
    Console.ReadKey(true) |> ignore
    0