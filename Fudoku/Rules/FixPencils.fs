module Fudoku.FixPencils

open Domain
open Puzzle

let fixPencilsRule lookup =
    let solveGroup group =
        let cellsInGroup = List.map lookup group

        let digitsInGroup =
            cellsInGroup
            |> List.map cellDigit
            |> List.fold FastSet.union NoDigits

        let pencilsToRemoveFromCell c = FastSet.intersect digitsInGroup (cellPencils c)

        cellsInGroup
        |> List.map (fun c -> c, pencilsToRemoveFromCell c)
        |> List.filter (fun (_, ds) -> (FastSet.length ds) > 0)
        |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

    let changes = List.collect solveGroup AllGroups

    { rule = "fix-pencils"; changes = changes }
