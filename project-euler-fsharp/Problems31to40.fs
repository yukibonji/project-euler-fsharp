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