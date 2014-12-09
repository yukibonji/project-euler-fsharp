module Common

open System
open System.IO
open System.Linq

let factorsOf (n: int64) =
       let root = n |> float |> sqrt |> int64
       seq { for x in 2L .. root do if n % x = 0L then yield x; yield n / x }

let isPrime n =
    factorsOf n |> Seq.isEmpty

let rec gcd a b =
    match a % b with
    | 0L -> b
    | n -> gcd b n

let (|Even|Odd|) i = 
    if i % 2L = 0L then Even else Odd

let readLines (problem: int) =
    File.ReadAllLines(String.Format("../../data/{0}.txt", problem)).Where(String.IsNullOrWhiteSpace >> not)

let charNumberToInt (c: char) =
    int c - 48