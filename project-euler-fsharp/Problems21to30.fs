module Problems21to30

open System
open System.Collections.Generic
open Common

let problem21 () =
    let factorsSum = seq { 0L .. 9999L }
                     |> Seq.map (factorsOf >> Seq.distinct >> Seq.sum >> (+) 1L >> int)
                     |> Seq.toArray
    let len = Array.length factorsSum

    let isAmicable i =
        let value = factorsSum.[i]
        value < len && value <> i && factorsSum.[value] = i
    
    seq { 0 .. 9999 }
    |> Seq.filter isAmicable
    |> Seq.sum

let problem22 () =
    let raw = readLines 22 |> Seq.nth 0

    raw.Split(',')
    |> Seq.map (fun s -> s.Substring(1, s.Length - 2))
    |> Seq.sort
    |> Seq.fold (fun (sum, i) s -> s |> Seq.sumBy (fun c -> int c - 64) |> (*) i |> (+) sum, i + 1) (0, 1)
    |> fst

let problem23 () =
    let abundantNumbers = seq { 1 .. 28123 }
                          |> Seq.filter (fun i -> (int64 >> factorsOf >> Seq.distinct >> Seq.sum >> int >> (+) 1) i > i)
                          |> Seq.toArray
    
    let sumFlags = Array.create 28124 false
    let limit = abundantNumbers
                |> Array.pick (fun i -> if abundantNumbers.[int i] > 28123 / 2 then Some(i) else None)

    abundantNumbers.[ 0 .. limit ]
    |> Array.iter (fun i -> abundantNumbers
                            |> Array.iter (fun j -> if i + j <= 28123 then sumFlags.[i + j] <- true))

    sumFlags
    |> Array.mapi (fun index v -> index, v)
    |> Array.filter (not << snd)
    |> Array.sumBy fst

let problem24 () =
    let curr = ref 999999
    let left = new List<int>([0 .. 9])
    let permutations = [ 1 .. 10 ] |> Seq.scan (*) 1 |> Seq.toArray

    let takeNthAvailable n =
        let res = left.[n]
        left.Remove(res) |> ignore
        res

    seq { for i in 0 .. 9 do
              yield !curr / permutations.[9 - i] |> takeNthAvailable |> (*) <| pown 10 (9 - i)
              curr := !curr % permutations.[9 - i]
    }
    |> Seq.sumBy int64

let problem25 () =
    let fibs = Seq.unfold (fun (prev, curr) -> Some(curr, (curr, (fst curr + fst prev, snd curr + 1)))) ((1I, 1), (1I, 2))
    let limit = 10I ** 999

    fibs
    |> Seq.skipWhile (fun i -> fst i < limit)
    |> Seq.head
    |> snd

let problem26 () =
    let repeatingDigitLength n =
        let rec divide remainder digits =
            let rem = remainder * 10
            match rem % n with
            | 0 -> 0
            | r -> let d = rem / n
                   match digits |> List.tryFindIndex ((=) (rem, d)) with
                   | Some(i) -> i + 1 
                   | None -> divide r ((rem, d) :: digits)  

        divide 1 []
    
    [| 1 .. 999 |]
    |> Array.Parallel.mapi (fun i n -> repeatingDigitLength n, i)
    |> Array.maxBy fst
    |> (snd >> (+) 1)

let problem27 () =
    let gen a b n =
        n * n + a * n + b

    let root = float >> sqrt >> int64

    let gensPrime a b n =
        let candidate = gen a b n

        match candidate with
        | x when x < 2L -> false
        | x -> seq { 2L .. root x}
               |> Seq.exists ((%) candidate >> (=) 0L)
               |> not
    
    let generatedPrimes (a, b) =
        Seq.initInfinite int64
        |> Seq.takeWhile (gensPrime a b)
        |> Seq.length

    let a, b = seq { for a in -999L .. 2L .. 999L do for b in 2L .. 999L do yield a, b }
               |> Seq.maxBy generatedPrimes

    a * b

let problem28 () =
    Seq.unfold (fun (sub, last) -> if last = 1 then None else Some(4 * (last - 3 * sub / 2), (sub - 2, last - 4 * sub))) (1000, 1001 * 1001)
    |> Seq.sum
    |> (+) 1