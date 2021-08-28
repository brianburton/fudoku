module Fudoku.BUG

open Domain
open Puzzle

let seekBUG (lookup: CellFinder) : Cell option =
    let summary =
        AllPositions
        |> List.map lookup
        |> List.map (fun cell -> (FastSet.length (cellPencils cell), cell))
        |> List.fold
            (fun (pairs, triples, others, triple) (count, cell) ->
                match count with
                | 0 -> (pairs, triples, others, triple)
                | 2 -> (pairs + 1, triples, others, triple)
                | 3 -> (pairs, triples + 1, others, Some cell)
                | _ -> (pairs, triples, others + 1, triple))
            (0, 0, 0, None)

    match summary with
    | pairs, _, _, _ when pairs = 0 -> None
    | _, _, others, _ when others <> 0 -> None
    | _, triples, _, _ when triples <> 1 -> None
    | _, _, _, triple -> triple

let solveBUG (lookup: CellFinder) (bug: Cell) : Position * RuleResultChange =
    let rowMap = createDigitMap ((rowNeighbors bug.position) @ [ bug.position ]) lookup

    let threeDigit =
        SetMap.keys rowMap
        |> List.map (fun digit -> digit, SetMap.getCount digit rowMap)
        |> List.find (fun (_, count) -> count = 3)
        |> fst

    bug.position, Solved threeDigit

let rule lookup =
    let changes =
        seekBUG lookup
        |> Option.map (solveBUG lookup)
        |> Option.toList

    { rule = "BUG"; changes = changes }
