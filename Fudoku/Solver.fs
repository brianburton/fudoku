module Fudoku.Solver

open Puzzle

type SolutionStep = { rule: string; puzzle: Puzzle }

let AllRules =
    [ FixPencils.rule
      SingleDigit.rule
      SingleCell.rule ]
    @ Fish.XWingRules
    @ Fish.SwordfishRules
    @ SingleBox.rules
      @ Tuple.nakedRules @ Tuple.hiddenRules

let fixPencils puzzle =
    let x =
        applyRules (cellFinder puzzle) [ FixPencils.rule ]

    applyRuleResults x.changes puzzle

let solvePuzzle puzzle =

    let rec loop priorSteps currentPuzzle =
        let results =
            applyRules (cellFinder currentPuzzle) AllRules

        if results.changes.Length = 0 then
            priorSteps
        else
            let newPuzzle =
                applyRuleResults results.changes currentPuzzle

            let thisStep =
                List.singleton
                    { rule = results.rule
                      puzzle = newPuzzle }

            let newSteps = priorSteps @ thisStep
            loop newSteps newPuzzle


    let initialSteps =
        List.singleton { rule = "start"; puzzle = puzzle }

    let finalSteps = loop initialSteps puzzle
    let finalPuzzle = (List.last finalSteps).puzzle
    (finalPuzzle, finalSteps)
