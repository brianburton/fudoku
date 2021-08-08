﻿module Fudoku.App

open System.IO
open Fudoku.Domain
open Fudoku.Solver
open Fudoku.Puzzle
open Fudoku.PuzzleIO

let printResults puzzle (steps: SolutionStep list) =
    let printPencilDiffs before after =
        let extra =
            if Set.isProperSuperset before after then
                $" - {digitsToString (Set.difference before after)}"
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

    let rec printSteps prev step tail =
        let diffs = diffPuzzles prev step.puzzle
        printfn $"Rule: %s{step.rule}"
        printDiffs diffs
        printfn $"%s{puzzleToString step.puzzle}"

        match tail with
        | nextStep :: nextTail -> printSteps step.puzzle nextStep nextTail
        | _ -> ()

    match steps with
    | step :: tail -> printSteps puzzle step tail
    | _ -> ()

exception BadArguments of string

[<EntryPoint>]
let main args =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    let puzzle =
        match args with
        | [| path |] -> readFileAsPuzzle path
        | [||] -> stringToPuzzle source
        | _ -> Error(BadArguments "usage: Fudoku path")

    match puzzle with
    | Ok barePuzzle ->
        let initializedPuzzle = fixPencils barePuzzle
        let _, steps = solvePuzzle initializedPuzzle
        printResults initializedPuzzle steps
    | Error e -> printfn $"error parsing puzzle: %s{e.ToString()}"

    0 // return an integer exit code
