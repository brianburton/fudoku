namespace Fudoku

type SetMap<'K, 'V when 'K: comparison and 'V: comparison> =
    private SetMap of FastMap<'K, FastSet<'V>>
        override this.ToString() =
            match this with
            | SetMap m -> $"{m}"

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

    let assign key values (SetMap setMap) =
        if FastSet.isEmpty values then
            FastMap.remove key setMap
        else
            FastMap.add key values setMap
        |> SetMap

    let filter predicate (SetMap setMap) =
        let folder map (k, vs) = if predicate k vs then map else FastMap.remove k map

        FastMap.toSeq setMap
        |> Seq.fold folder setMap
        |> SetMap

    let map mapping (SetMap setMap) =
        FastMap.toSeq setMap
        |> Seq.map (fun (k, vs) -> k, (mapping vs))
        |> Seq.filter (fun (_, vs) -> FastSet.length vs > 0)
        |> FastMap.ofSeq
        |> SetMap

    let map2 mapping (SetMap setMap) =
        FastMap.toSeq setMap
        |> Seq.map (fun (k, vs) -> mapping k vs)
        |> Seq.filter (fun (_, vs) -> FastSet.length vs > 0)
        |> FastMap.ofSeq
        |> SetMap

    let removeKey key (SetMap setMap) = setMap |> FastMap.remove key |> SetMap

    let get key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.defaultValue (FastSet.empty ())

    let contains key value setMap = get key setMap |> FastSet.contains value

    let getCount key (SetMap setMap) =
        FastMap.tryFind key setMap
        |> Option.map FastSet.length
        |> Option.defaultValue 0

    let length (SetMap setMap) = FastMap.length setMap

    let keys (SetMap setMap) =
        setMap
        |> FastMap.toSeq
        |> Seq.map fst
        |> List.ofSeq

    let folder splitter =
        fun setMap raw ->
            let key, value = splitter raw
            setMap |> add key value

    let ofList splitter list = list |> List.fold (folder splitter) (empty ())

    let ofPairs list =
        let folder map (k, v) = add k v map
        list |> Seq.fold folder (empty ())

    let toSeq (SetMap setMap) = FastMap.toSeq setMap

    let ofSeq seq =
        seq
        |> Seq.filter (fun (_, values) -> FastSet.length values > 0)
        |> FastMap.ofSeq
        |> SetMap

    let intersectKeys ks setMap =
        match Seq.toList ks with
        | [] -> FastSet.empty ()
        | [k] -> get k setMap
        | k::tail ->
            tail
            |> Seq.map (fun kk -> get kk setMap)
            |> Seq.fold FastSet.intersect (get k setMap)

    let unionKeys ks setMap =
        ks
        |> Seq.map (fun k -> get k setMap)
        |> Seq.fold FastSet.union (FastSet.empty ())
