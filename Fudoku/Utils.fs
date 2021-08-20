module Fudoku.Utils

open System.Linq
open FSharpx.Collections

let memoize fn =
    let cache = System.Collections.Generic.Dictionary<_, _>()

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

let listToOption list = if List.isEmpty list then None else Some list

let intersectLists (xs: 'a seq) (ys: 'a seq) = xs.Intersect(ys) |> List.ofSeq

let setsOverlap (a: Set<'a>) (b: Set<'a>) =
    let common = Set.intersect a b
    (Set.count common) > 0

let setContainsElement set = fun e -> Set.contains e set

type FastMap<'K,'V when 'K: equality and 'V: equality> = PersistentHashMap<'K,'V>

module FastMap =
    let empty ()  = PersistentHashMap.empty

    let containsKey k m = PersistentHashMap.containsKey k m

    let add k v m = PersistentHashMap.add k v m

    let remove k m = PersistentHashMap.remove k m

    let find k m = PersistentHashMap.find k m

    let tryFind k m =
        if PersistentHashMap.containsKey k m then
            Some (PersistentHashMap.find k m)
        else
            None

    let change k f m =
        match f (tryFind k m) with
        | Some v -> add k v m
        | None -> remove k m

    let ofSeq = PersistentHashMap.ofSeq

    let ofList = ofSeq

    let toSeq = PersistentHashMap.toSeq

    let toList m = m |> toSeq |> List.ofSeq

type SetMap<'K, 'V when 'K: comparison and 'V: comparison> = private SetMap of FastMap<'K, Set<'V>>

module SetMap =
    let empty () = SetMap (FastMap.empty ())

    let add key value (SetMap setMap) =
        let adder vs =
            match vs with
            | Some set -> Set.add value set
            | None -> Set.singleton value
            |> Some

        setMap |> FastMap.change key adder |> SetMap

    let remove key value (SetMap setMap) =
        let remover vs =
            vs
            |> Option.map (Set.remove value)
            |> Option.filter (fun set -> not set.IsEmpty)

        setMap |> FastMap.change key remover |> SetMap

    let get key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.defaultValue Set.empty

    let contains key value setMap = get key setMap |> Set.contains value

    let getCount key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.map Set.count
        |> Option.defaultValue 0

    let keys (SetMap setMap) = setMap |> FastMap.toList |> List.map fst

    let folder splitter =
        fun setMap raw ->
            let key, value = splitter raw
            setMap |> add key value

    let ofList splitter list = list |> List.fold (folder splitter) (empty ())

    let ofPairs list =
        let folder map (k, v) = add k v map
        list |> List.fold folder (empty ())
