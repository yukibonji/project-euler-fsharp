﻿module Common

open System

let factorsOf (n: int64) =
       let root = n |> float |> sqrt |> int64
       seq { for x in 2L .. root do if n % x = 0L then yield x }

let isPrime n =
    factorsOf n |> Seq.isEmpty

let rec gcd a b =
    match a % b with
    | 0L -> b
    | n -> gcd b n