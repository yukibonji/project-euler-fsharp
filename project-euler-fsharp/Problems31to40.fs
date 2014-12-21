module Problems31to40

open System
open Common

let problem32() =
    let isPandigitalProduct (a, b) =
        let str = sprintf "%d%d%d" a b (a * b)
        if Seq.length str <> 9 || Seq.exists ((=) '0') str
        then false
        else Seq.distinct str |> Seq.length |> (=) <| Seq.length str

    seq { for i in 2 .. 9999 do for j in 2 .. 10000 / i do yield i, j }
    |> Seq.filter isPandigitalProduct
    |> Seq.map (apply (*))
    |> Seq.distinct
    |> Seq.sum

let problem33 () =
    let isUnusual (n, d) =
        let value = float n / float d
        let ndiv = n / 10 |> float
        let nmod = n % 10 |> float
        let ddiv = d / 10 |> float
        let dmod = d % 10 |> float

        if nmod = 0.0 || dmod = 0.0 then false
        elif nmod <> ddiv && ndiv <> dmod then false
        elif nmod / ddiv = value then true
        elif ndiv / dmod = value then true
        else false

    let n, d = seq { for denominator in 11 .. 99 do for nominator in 10 .. denominator - 1 do yield nominator, denominator }
               |> Seq.filter isUnusual
               |> Seq.reduce (fun (n1, d1) (n2, d2) -> n1 * n2, d1 * d2)

    d / gcd n d

let problem34 () =
    let factorials = [ 1 .. 9 ] |> List.scan (*) 1 |> List.toArray

    let isSumOfDigitFactorials n =
        string n |> Seq.sumBy (fun i -> factorials.[i |> charNumberToInt]) = n

    seq { 3 .. 7 * factorials.[9] }
    |> Seq.filter isSumOfDigitFactorials
    |> Seq.sum

let problem35 () =
    let root = float 999999 |> sqrt |> int
    let sieve = ref [ 3 .. 2 .. 999999 ]

    for i in 3 .. root do
        sieve := List.filter (fun x -> x = i || x % i <> 0) !sieve

    let rotations (n: string) =
        [ 0 .. Seq.length n - 1 ]
        |> List.map (fun i -> sprintf "%s%s" n.[ i .. ] n.[ 0 .. i - 1 ] |> int)

    let doesNotContainDigits (n: string) =
        [ "0"; "2"; "4"; "6"; "8" ]
        |> Seq.forall (fun i -> not <| n.Contains(i))

    seq { 3 .. 2 .. 999999 }
    |> Seq.map string
    |> Seq.filter doesNotContainDigits
    |> Seq.filter (rotations >> List.forall (fun i -> List.exists ((=) i) !sieve))
    |> Seq.length
    |> (+) 1

let problem36 () =
    let isPalindrome n =
        let str = string n
        let bin = toBinary n
        str = new string(str |> Seq.toArray |> Array.rev) && bin = List.rev bin

    seq { 1 .. 2 .. 999999 }
    |> Seq.filter isPalindrome
    |> Seq.sum