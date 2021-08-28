module Fudoku.SingleDigit

open Domain
open Puzzle

let private singlePencilRule lookup group =
    let changes =
        group
        |> List.map lookup
        |> List.map (fun c -> (c, cellPencils c))
        |> List.filter (fun (_, ds) -> (FastSet.length ds) = 1)
        |> List.map (fun (c, ds) -> c.position, Solved(FastSet.head ds))

    { rule = "single-pencil"; changes = changes }

let rule lookup = singlePencilRule lookup AllPositions
