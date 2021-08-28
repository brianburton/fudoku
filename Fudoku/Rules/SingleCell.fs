module Fudoku.SingleCell

open Domain
open Puzzle

let private singleCellPencil (lookup: CellFinder) (group: Position list) =
    let map = createDigitMap group lookup

    SetMap.toSeq map
    |> Seq.filter (fun (_, ps) -> FastSet.length ps = 1)
    |> Seq.map (fun (d, ps) -> FastSet.head ps, Solved d)
    |> Seq.toList

let rule lookup =
    let changes =
        AllGroups
        |> List.collect (singleCellPencil lookup)

    { rule = "single-cell"; changes = changes }
