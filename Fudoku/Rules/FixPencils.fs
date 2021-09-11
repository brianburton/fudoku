module Fudoku.FixPencils

open Domain
open Puzzle

let fixPencilsRule lookup =
    let solveGroup group =
        let cellsInGroup = group |> List.map lookup

        let digitsInGroup =
            cellsInGroup
            |> List.map cellDigit
            |> List.fold FastSet.union NoDigits

        let pencilsToRemoveFromCell c = FastSet.intersect digitsInGroup (cellPencils c)

        cellsInGroup
        |> List.map (fun c -> c.position, pencilsToRemoveFromCell c)
        |> List.filter (fun (_, ds) -> (FastSet.length ds) > 0)
        |> List.map (fun (p, ds) -> p, RemovePencils ds)

    let changes = AllGroups |> List.collect solveGroup

    { rule = "fix-pencils"; changes = changes }
