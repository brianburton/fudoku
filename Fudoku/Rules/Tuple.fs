module Fudoku.Tuple

open Utils
open Domain
open Puzzle

let solveTuple size lookup =
    let toRule name changes =
        let suffix =
            match size with
            | 2 -> "pair"
            | 3 -> "triple"
            | 4 -> "quad"
            | _ -> $"tuple-{size}"

        { rule = $"{name}-{suffix}"; changes = changes }

    let solveGroup group =
        let summaries =
            group
            |> List.map (summarizeCell lookup)
            |> List.filter (fun s -> FastSet.length s.cellPencils > 0)

        let groupDigitMap = createDigitMap group lookup

        let groupDigits =
            SetMap.toSeq groupDigitMap
            |> Seq.collect (fun (d, ps) -> if FastSet.length ps > 1 then [ d ] else [])
            |> List.ofSeq

        let digitCombinations = combinations size groupDigits |> Seq.ofList

        let solveForDigits digits =
            let digitSet = FastSet.ofSeq digits

            let nakeds =
                summaries
                |> List.filter (fun s -> FastSet.isSuperset digitSet s.cellPencils)

            if List.length nakeds = size then
                // this is a naked tuple
                let nakedPositions =
                    nakeds
                    |> List.map (fun s -> s.cellPos)
                    |> FastSet.ofSeq

                let others =
                    summaries
                    |> List.filter (fun s -> not (FastSet.contains s.cellPos nakedPositions))
                    |> List.filter (fun s -> FastSet.overlaps digitSet s.cellPencils)

                others
                |> List.map (fun s -> s.cellPos, RemovePencils digitSet)
                |> toRule "naked"
            else
                let supersets =
                    summaries
                    |> List.filter (fun s -> FastSet.overlaps s.cellPencils digitSet)

                if List.length supersets = size then
                    // this is a hidden tuple
                    let changes =
                        supersets
                        |> List.filter (fun s -> FastSet.length (FastSet.difference s.cellPencils digitSet) > 0)
                        |> List.map (fun s -> s.cellPos, RetainPencils(FastSet.intersect digitSet s.cellPencils))

                    changes |> toRule "hidden"
                else
                    toRule "no-tuple" []

        digitCombinations |> Seq.map solveForDigits

    AllGroups
    |> Seq.ofList
    |> Seq.collect solveGroup
    |> Seq.tryFind isNonEmptyResult
    |> Option.defaultValue (toRule "no-tuple" [])
