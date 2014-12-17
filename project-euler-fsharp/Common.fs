module Common

open System
open System.IO
open System.Linq
open System.Collections.Generic

let factorsOf (n: int64) =
       let root = n |> float |> sqrt |> int64
       seq { for x in 2L .. root do if n % x = 0L then yield x; yield n / x }

let isPrime n =
    factorsOf n |> Seq.isEmpty

let inline gcd a b =
    let rec innerGcd a b =
        let n = a % b
        if n = LanguagePrimitives.GenericZero then b else innerGcd b n

    innerGcd a b

let (|Even|Odd|) i =
    if i % 2L = 0L then Even else Odd

let readLines (problem: int) =
    File.ReadAllLines(String.Format("../../data/{0}.txt", problem)).Where(String.IsNullOrWhiteSpace >> not)

let charNumberToInt (c: char) =
    int c - 48

let swapIndexes i j (array: 'a []) =
    let tmp = array.[i]
    array.[i] <- array.[j]
    array.[j] <- tmp

let apply f (a, b) =
    f a b

type Memoizer<'T, 'U when 'T: equality>() =
    let cache = Dictionary<'T, 'U>()

    member this.Get n =
        match cache.TryGetValue(n) with
        | true, value -> Some(value)
        | false, _ -> None

    member this.Store key value =
        cache.Add(key, value)