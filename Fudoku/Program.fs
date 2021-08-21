﻿module Fudoku.App

open Fudoku.Utils
open Fudoku.Domain
open Fudoku.Solver
open Fudoku.Puzzle
open Fudoku.PuzzleIO

let printResult (prev:Puzzle) (step: SolutionStep) =
    let printPencilDiffs before after =
        let extra =
            if FastSet.isProperSuperset before after then
                $" - {digitsToString (FastSet.difference before after)}"
            else
                ""

        $"{digitsToString before}{extra} -> {digitsToString after}"

    let rec printDiffs diffs =
        let printDiff diff tail =
            match diff with
            | { diffPosition = p
                before = _
                after = Answer d } -> printfn $"   {p}: Solved {digitToString d}"
            | { diffPosition = p
                before = Pencils b
                after = Pencils a } -> printfn $"   {p}: {printPencilDiffs b a}"
            | { diffPosition = p
                before = b
                after = a } -> printfn $"   {p}: ??? {b}->{a}"

            printDiffs tail

        match diffs with
        | diff :: tail -> printDiff diff tail
        | _ -> ()

    let diffs = diffPuzzles prev step.puzzle
    printfn $"Rule: %s{step.rule}"
    printDiffs diffs
    printfn $"%s{puzzleToString step.puzzle}"
    step.puzzle

exception BadArguments of string

[<EntryPoint>]
let main args =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    let puzzle =
        match args with
        | [| path |] -> readFileAsPuzzle path
        | [||] -> stringToPuzzle source
        | _ -> Error "usage: Fudoku path"

    match puzzle with
    | Ok barePuzzle ->
        let initializedPuzzle = fixPencils barePuzzle
        solvePuzzle initializedPuzzle
        |> Seq.fold printResult initializedPuzzle
        |> ignore
    | Error e -> printfn $"error: %s{e}"

    0 // return an integer exit code
