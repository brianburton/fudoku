module Fudoku.App

open Fudoku.Puzzle
open Fudoku.PuzzleIO

[<EntryPoint>]
let main _ =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    let pz = stringToPuzzle source
    let solved = solvePuzzle pz

    0 // return an integer exit code
