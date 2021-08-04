module Fudoku.App

[<EntryPoint>]
let main _ =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    match PuzzleIO.stringToPuzzle source with
    | Ok pz ->
        Solver.solvePuzzle pz |> ignore
    | Error e ->
        printfn $"error parsing puzzle: %s{e.Message}"

    0 // return an integer exit code
