module Fudoku.Solver

open Domain
open Puzzle

type SolutionStep = { rule: string; puzzle: Puzzle }

let AllRules =
    [ FixPencils.fixPencilsRule
      SingleDigit.rule
      SingleCell.rule
      SingleBox.rule
      Tuple.nakedPairsRule
      Tuple.hiddenPairsRule
      Rectangle.uniqueRectangleRule
      Fish.xWingRule
      DigitChain.rule
      Tuple.nakedTriplesRule
      Tuple.hiddenTriplesRule
      BUG.rule
      Tuple.nakedQuadsRule
      Tuple.hiddenQuadsRule
      Fish.swordfishRule ]

let fixPencils puzzle =
    let x = applyRules (cellFinder puzzle) [ FixPencils.fixPencilsRule ]

    applyRuleResults x.changes puzzle

let private selectRules answered = if answered then AllRules else List.tail AllRules

let private includesAnswer result =
    let isAnswer =
        fun change ->
            match change with
            | Solved _ -> true
            | _ -> false

    result.changes
    |> Seq.tryFind (fun (_, ch) -> isAnswer ch)
    |> Option.isSome

let private findStepChanges fixNeeded currentPuzzle =
    let rules = selectRules fixNeeded
    let results = applyRules (cellFinder currentPuzzle) rules

    match results.changes with
    | [] -> false, { rule = ""; puzzle = currentPuzzle }
    | changes ->
        let newPuzzle = applyRuleResults changes currentPuzzle
        let newFixNeeded = includesAnswer results
        let step = { rule = results.rule; puzzle = newPuzzle }
        newFixNeeded, step

let solvePuzzle puzzle =
    let startRule = { rule = "start"; puzzle = puzzle }

    seq { for i in 1 .. 1000 -> i }
    |> Seq.scan (fun (fixNeeded, step) _ -> findStepChanges fixNeeded step.puzzle) (false, startRule)
    |> Seq.takeWhile (fun (_, step) -> step.rule.Length > 0)
    |> Seq.map snd
