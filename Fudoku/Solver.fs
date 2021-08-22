module Fudoku.Solver

open Puzzle

type SolutionStep = { rule: string; puzzle: Puzzle }

let AllRules =
    [ FixPencils.rule
      SingleDigit.rule
      SingleCell.rule
      SingleBox.rule
      Tuple.nakedRule
      Tuple.hiddenRule
      Rectangle.uniqueRectangleRule
      Fish.xWingRule
      DigitChain.rule
      BUG.rule
      Fish.swordfishRule]

let fixPencils puzzle =
    let x = applyRules (cellFinder puzzle) [ FixPencils.rule ]

    applyRuleResults x.changes puzzle

let private findStepChanges currentPuzzle =
    let results = applyRules (cellFinder currentPuzzle) AllRules

    match results.changes with
    | [] -> { rule = ""; puzzle = currentPuzzle }
    | changes ->
        let newPuzzle = applyRuleResults changes currentPuzzle
        { rule = results.rule; puzzle = newPuzzle }

let solvePuzzle puzzle =
    seq { for i in 1 .. 1000 -> i }
    |> Seq.scan (fun step _ -> findStepChanges step.puzzle) { rule = "start"; puzzle = puzzle }
    |> Seq.takeWhile (fun step -> step.rule.Length > 0)
