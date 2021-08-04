namespace Fudoku

open Domain
open Puzzle

module SingleDigit =
    let singlePencilRule lookup group =
        let changes =
            group
            |> List.map lookup
            |> List.map (fun c -> (c, cellPencils c))
            |> List.filter (fun (_, ds) -> ds.Count = 1)
            |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

        { rule = "single-pencil"
          changes = changes }

    let rule lookup = singlePencilRule lookup AllPositions
