namespace Fudoku

open FSharpx.Collections

type FastMap<'K, 'V when 'K: equality and 'V: equality> =
    private
    | FastMap of PersistentHashMap<'K, 'V>
    override this.ToString() =
        match this with
        | FastMap m ->
            let str =
                PersistentHashMap.toSeq m
                |> Seq.map (fun (k, v) -> $"({k},{v})")
                |> Seq.fold (fun s p -> $"{s},{p}") ""

            $"[{str.Substring(1)}]"

module FastMap =
    let empty () = FastMap PersistentHashMap.empty

    let singleton k v =
        PersistentHashMap.empty
        |> PersistentHashMap.add k v
        |> FastMap

    let containsKey k (FastMap m) = PersistentHashMap.containsKey k m

    let add k v (FastMap m) = PersistentHashMap.add k v m |> FastMap

    let remove k (FastMap m) = PersistentHashMap.remove k m |> FastMap

    let find k (FastMap m) = PersistentHashMap.find k m

    let tryFind k (FastMap m) =
        if PersistentHashMap.containsKey k m then
            Some(PersistentHashMap.find k m)
        else
            None

    let change k f m =
        match f (tryFind k m) with
        | Some v -> add k v m
        | None -> remove k m

    let length (FastMap m) = PersistentHashMap.length m

    let ofSeq seq = seq |> PersistentHashMap.ofSeq |> FastMap

    let ofList = ofSeq

    let toSeq (FastMap m) = m |> PersistentHashMap.toSeq

    let toList m = m |> toSeq |> List.ofSeq

    let keys m = m |> toSeq |> Seq.map fst
