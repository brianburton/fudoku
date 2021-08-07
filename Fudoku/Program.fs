module Fudoku.App

open Fudoku.Domain
open Fudoku.Solver
open Fudoku.Puzzle
open Fudoku.PuzzleIO

let printResults puzzle (steps: SolutionStep list) =
    let rec printDiffs diffs =
        let printDiff diff tail =
            match diff with
            | { diffPosition = p
                before = _
                after = Answer d } -> printfn $"   {positionToString p}: Solved {digitToString d}"
            | { diffPosition = p
                before = Pencils b
                after = Pencils a } -> printfn $"   {positionToString p}: {digitsToString b} -> {digitsToString a}"
            | { diffPosition = p
                before = b
                after = a } -> printfn $"   {positionToString p}: ??? {b}->{a}"
            printDiffs tail

        match diffs with
        | diff::tail -> printDiff diff tail
        | _ -> ()

    let rec printSteps prev step tail =
        let diffs = diffPuzzles prev step.puzzle
        printfn $"Rule: %s{step.rule}"
        printDiffs diffs
        printfn $"%s{puzzleToString step.puzzle}"
        match tail with
        | nextStep::nextTail -> printSteps step.puzzle nextStep nextTail
        | _ -> ()

    match steps with
    | step::tail -> printSteps puzzle step tail
    | _ -> ()

[<EntryPoint>]
let main _ =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    match stringToPuzzle source with
    | Ok pz ->
        let result, steps = Solver.solvePuzzle pz
        printResults pz steps
    | Error e -> printfn $"error parsing puzzle: %s{e.Message}"

    0 // return an integer exit code
