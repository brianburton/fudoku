module Fudoku.Puzzle

open Fudoku.Domain

type PuzzleSolution = Map<Position, Digit>
type Puzzle = Map<Position, Cell>

type RuleResultChange =
    | Solved of Digit
    | RemovePencils of Set<Digit>
    | RetainPencils of Set<Digit>

type RuleResult =
    { rule: string
      changes: (Position * RuleResultChange) list }

type Rule = CellFinder -> RuleResult

let emptyPuzzle: Puzzle =
    AllPositions
    |> List.map (fun p -> p, starterCell p)
    |> Map.ofList

let addToPuzzle (pz: Puzzle) (cells: Cell list) : Puzzle =
    let addCell puzzle cell = Map.add cell.position cell puzzle

    cells |> List.fold addCell pz

let isCompleteSolution (s: PuzzleSolution) = s.Count = AllPositions.Length

let cellFinder pz = fun p -> Map.find p pz

let applyRuleResults results puzzle =
    let solve p d pz = pz |> Map.add p (solvedCell p d)

    let updatePencils p ds pz op =
        let currentPencils = cellPencils (Map.find p pz)
        let changedPencils = op currentPencils ds
        let changedCell = unsolvedCell p changedPencils
        Map.add p changedCell pz

    let remove p ds pz = updatePencils p ds pz Set.difference

    let retain p ds pz = updatePencils p ds pz Set.intersect

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemovePencils ds -> remove p ds pz
        | RetainPencils ds -> retain p ds pz

    let applied = List.fold applyResult puzzle results

    applied

let applyRules lookup rules =
    let applyRule prior rule =
        if List.isEmpty prior.changes then
            rule lookup
        else
            prior

    let emptyResult = { rule = ""; changes = List.empty }

    rules |> List.fold applyRule emptyResult
