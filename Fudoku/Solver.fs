module Fudoku.Solver

open Puzzle

type SolutionStep = { rule: string; puzzle: Puzzle }

let AllRules =
    [ FixPencils.rule; SingleDigit.rule ]
    @ SingleBox.rules
    @ Tuple.singleCellRules
      @ Tuple.nakedRules @ Tuple.hiddenRules

let solvePuzzle puzzle =
    let applyAllRulesToPuzzle pz = applyRules (cellFinder pz) AllRules

    let mutable steps =
        List.singleton { rule = "start"; puzzle = puzzle }

    let mutable puzzle2 = puzzle
    let mutable results = applyAllRulesToPuzzle puzzle2

    while results.changes.Length > 0 do
        puzzle2 <- applyRuleResults results.changes puzzle2

        let step =
            { rule = results.rule
              puzzle = puzzle2 }

        steps <- steps @ [ step ]
        results <- applyAllRulesToPuzzle puzzle2

    (puzzle2, steps)
