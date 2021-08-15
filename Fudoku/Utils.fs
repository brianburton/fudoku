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
    | [ single ] -> Some(single, [])
    | head :: _ -> Some(head, List.except [ head ] list)

let intersectLists (xs: 'a seq) (ys: 'a seq) = xs.Intersect(ys) |> List.ofSeq

let setsOverlap (a: Set<'a>) (b: Set<'a>) =
    let common = Set.intersect a b
    (Set.count common) > 0

let setContainsElement set =
    fun e -> Set.contains e set

module SetMap =
    let empty = Map.empty

    let add key value setMap =
        let adder vs =
            match vs with
            | Some set -> Set.add value set
            | None -> Set.singleton value
            |> Some

        setMap |> Map.change key adder

    let remove key value setMap =
        let remover vs =
            vs
            |> Option.map (Set.remove value)
            |> Option.filter (fun set -> not set.IsEmpty)

        setMap |> Map.change key remover

    let get key setMap =
        Map.tryFind key setMap
        |> Option.defaultValue Set.empty

    let contains key value setMap = get key setMap |> Set.contains value

    let keys setMap = setMap |> Map.toList |> List.map fst

    let folder splitter =
        fun setMap raw ->
            let key,value = splitter raw
            setMap |> add key value

    let ofList splitter list =
        list |> List.fold (folder splitter) empty

    let ofPairs list =
        let folder map (k,v) = add k v map
        list |> List.fold folder empty

