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