module Fudoku.Utils

open System.Linq

let memoize fn =
    let cache =
        System.Collections.Generic.Dictionary<_, _>()

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = fn (x)
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

    if len = 1 then length1 ()
    elif len = list.Length then single ()
    else normal list.Head list.Tail

let findAndRemove (list: 'a list) (condition: 'a -> bool) =
    let found = List.filter condition list

    match found with
    | [] -> None
    | [single]  -> Some(single, [])
    | head :: _ -> Some(head, List.except [head] list)

let intersectLists (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys) |> List.ofSeq

let setsOverlap (a:Set<'a>) (b:Set<'a>) =
    let common = Set.intersect a b
    (Set.count common) > 0
