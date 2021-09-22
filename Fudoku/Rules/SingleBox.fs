module Fudoku.SingleBox

open Utils
open Domain
open Puzzle

let private Combos =
    // list of functions that map a column to all positions in that column
    let colMappers = AllDigits |> List.map (fun r c -> position r c)

    // list of functions that map a row to all positions in that row
    let rowMappers = AllDigits |> List.map (fun c r -> position r c)

    let allMappers = colMappers @ rowMappers

    List.allPairs allMappers SingleSegmentDigitTriples
    |> List.map (fun (mapper, combo) -> combinationMapper mapper combo)

let private solveSingleBox (combo: Combination<Position>) (lookup: CellFinder) =
    let insideCells: Cell list = combo.inside |> List.map lookup

    let outsideCells: Cell list = combo.outside |> List.map lookup

    let insidePencils = groupPencils insideCells
    let outsidePencils = groupPencils outsideCells

    let uniquePencils = FastSet.difference insidePencils outsidePencils

    let isAllInBox = FastSet.length uniquePencils > 0

    let boxCellsToChange () =
        let first = List.head insideCells

        let neighbors =
            boxNeighbors first.position
            |> List.except combo.inside
            |> List.map lookup
            |> List.filter (cellContainsPencils uniquePencils)

        neighbors
        |> List.map (fun c -> c.position, RemovePencils uniquePencils)

    if isAllInBox then boxCellsToChange () else List.empty

let rule (lookup: CellFinder) =
    let changes =
        Seq.ofList Combos
        |> Seq.map (fun c -> solveSingleBox c lookup)
        |> firstNonEmptyList

    { rule = "single-box"; changes = changes }
