module Fudoku.Utils

open System.Linq

// swaps order of two arg function for use in filtering
let swapArgs fn a b = fn b a

// pass the values of a pair to a two arg function
let unpair fn (a,b) = fn a b

let memoize fn =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = fn x
            cache.Add(x, v)
            v)

let rec combinations len list =
    let length1 () = list |> List.map (fun x -> [ x ])

    let single () = [ list ]

    let merge item lst = lst |> List.map (fun x -> item :: x)

    let normal head tail =
        let tails = combinations len tail
        let suffixes = combinations (len - 1) tail
        let merges = merge head suffixes
        List.append merges tails

    if List.length list < len then []
    elif len = 1 then length1 ()
    elif len = list.Length then single ()
    else normal list.Head list.Tail

let intersectLists (xs: 'a seq) (ys: 'a seq) = xs.Intersect(ys) |> List.ofSeq

let isNonEmptyList xs = not (List.isEmpty xs)

let firstNonEmptyList seqOfLists =
    seqOfLists
    |> Seq.tryFind isNonEmptyList
    |> Option.defaultValue []
