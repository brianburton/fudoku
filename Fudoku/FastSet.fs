namespace Fudoku

type FastSet<'T when 'T: equality> =
    private
    | FastSet of FastMap<'T, bool>
    override this.ToString() =
        let str =
            match this with
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

    let bind f set =
        toSeq set
        |> Seq.collect (fun x -> toSeq (f x))
        |> ofSeq

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
