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

    if List.length list < len then []
    elif len = 1 then length1 ()
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

type FastMap<'K, 'V when 'K: equality and 'V: equality> = PersistentHashMap<'K, 'V>

module FastMap =
    let empty () = PersistentHashMap.empty

    let singleton k v =
        PersistentHashMap.empty
        |> PersistentHashMap.add k v

    let containsKey k m = PersistentHashMap.containsKey k m

    let add k v m = PersistentHashMap.add k v m

    let remove k m = PersistentHashMap.remove k m

    let find k m = PersistentHashMap.find k m

    let tryFind k m =
        if PersistentHashMap.containsKey k m then
            Some(PersistentHashMap.find k m)
        else
            None

    let change k f m =
        match f (tryFind k m) with
        | Some v -> add k v m
        | None -> remove k m

    let length m = PersistentHashMap.length m

    let ofSeq = PersistentHashMap.ofSeq

    let ofList = ofSeq

    let toSeq = PersistentHashMap.toSeq

    let toList m = m |> toSeq |> List.ofSeq

    let keys m = m |> toSeq |> Seq.map fst

type FastSet<'T when 'T: equality> =
    private FastSet of FastMap<'T, bool>
        override this.ToString() =
            let str = match this with
                        | FastSet m ->
                            FastMap.keys m
                            |> Seq.map (fun k -> $"{k}")
                            |> Seq.fold (fun s k -> $"{s},{k}") ""
            $"({str.Substring(1)})"

module FastSet =
    let toSeq (FastSet set) = FastMap.keys set

    let empty () = FastMap.empty () |> FastSet

    let singleton x = FastMap.singleton x true |> FastSet

    let isEmpty (FastSet set) = FastMap.length set = 0

    let add x (FastSet set) = FastMap.add x true set |> FastSet

    let addAll (FastSet set) xs =
        xs
        |> Seq.fold (fun s x -> FastMap.add x true s) set
        |> FastSet

    let ofSeq xs = xs |> addAll (empty ())

    let remove x (FastSet set) = FastMap.remove x set |> FastSet

    let except xs (FastSet set) =
        xs
        |> Seq.fold (fun s x -> FastMap.remove x s) set
        |> FastSet

    let contains x (FastSet set) = FastMap.containsKey x set

    let containsAll (FastSet set) xs =
        xs
        |> Seq.forall (fun x -> FastMap.containsKey x set)

    let containsAny (FastSet set) xs =
        xs
        |> Seq.tryFind (fun x -> FastMap.containsKey x set)
        |> Option.isSome

    let overlaps set other = containsAny set (toSeq other)

    let length (FastSet set) = FastMap.length set
    let head (FastSet set) = FastMap.keys set |> Seq.head

    let private biggerFirst a b = if (length a) >= (length b) then (a, b) else (b, a)

    let intersect a b =
        let bigger, smaller = biggerFirst a b

        let extra =
            toSeq smaller
            |> Seq.filter (fun x -> not (contains x bigger))

        except extra smaller

    let union a b =
        let bigger, smaller = biggerFirst a b

        toSeq smaller |> addAll bigger

    let difference a b = except (toSeq b) a

    let map f set = toSeq set |> Seq.map f |> ofSeq

    let bind f set = toSeq set |> Seq.collect (fun x -> toSeq (f x)) |> ofSeq

    let filter f set =
        let extra = toSeq set |> Seq.filter (fun x -> not (f x))
        except extra set

    let toFilter set = (fun x -> contains x set)

    let isSuperset set other = toSeq other |> containsAll set

    let isProperSuperset set other =
        (length set) > (length other)
        && (isSuperset set other)

    let equals set other =
        (length set) = (length other)
        && (isSuperset set other)

    let notEquals set other =
        (length set) <> (length other)
        || not (isSuperset set other)

    let toList set = toSeq set |> List.ofSeq

type SetMap<'K, 'V when 'K: comparison and 'V: comparison> = private SetMap of FastMap<'K, FastSet<'V>>

module SetMap =
    let empty () = SetMap(FastMap.empty ())

    let add key value (SetMap setMap) =
        let adder vs =
            match vs with
            | Some set -> FastSet.add value set
            | None -> FastSet.singleton value
            |> Some

        setMap |> FastMap.change key adder |> SetMap

    let remove key value (SetMap setMap) =
        let remover vs =
            vs
            |> Option.map (FastSet.remove value)
            |> Option.filter (fun set -> not (FastSet.isEmpty set))

        setMap |> FastMap.change key remover |> SetMap

    let removeKey key (SetMap setMap) =
        setMap |> FastMap.remove key |> SetMap

    let get key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.defaultValue (FastSet.empty ())

    let contains key value setMap = get key setMap |> FastSet.contains value

    let getCount key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.map FastSet.length
        |> Option.defaultValue 0

    let keys (SetMap setMap) = setMap |> FastMap.toList |> List.map fst

    let folder splitter =
        fun setMap raw ->
            let key, value = splitter raw
            setMap |> add key value

    let ofList splitter list = list |> List.fold (folder splitter) (empty ())

    let ofPairs list =
        let folder map (k, v) = add k v map
        list |> Seq.fold folder (empty ())

    let toSeq (SetMap setMap) = FastMap.toSeq setMap
