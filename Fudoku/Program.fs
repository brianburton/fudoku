module Fudoku.App

open Fudoku.Puzzle
open Fudoku.PuzzleIO

[<EntryPoint>]
let main _ =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    match stringToPuzzle source with
        | Ok pz ->
            solvePuzzle pz
            ()
        | Error e ->
            printfn $"error parsing puzzle: %s{e.Message}"
            ()

    0 // return an integer exit code
