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

type SolutionStep = { rule: string; puzzle: Puzzle }

let emptyPuzzle : Puzzle =
    allPositions
    |> List.map (fun p -> p, starterCell p)
    |> Map.ofList

let isCompleteSolution (s: PuzzleSolution) = s.Count = allPositions.Length

let cellFinder pz = fun p -> Map.find p pz

let applyRuleResults results puzzle =
    let solve p d pz = pz |> Map.add p (solvedCell p d)

    let pencils p ds pz op =
        let currentPencils = cellPencils (Map.find p pz)
        let changedPencils = ds |> op currentPencils
        let changedCell = unsolvedCell p changedPencils
        Map.add p changedCell pz

    let remove p ds pz = pencils p ds pz Set.difference

    let retain p ds pz = pencils p ds pz Set.intersect

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemovePencils ds -> remove p ds pz
        | RetainPencils ds -> retain p ds pz

    let applied = results |> List.fold applyResult puzzle

    applied

let applyRules lookup rules =
    let applyRule prior rule =
        if List.isEmpty prior.changes then
            rule lookup
        else
            prior

    rules
    |> List.fold applyRule { rule = ""; changes = List.empty }


let singlePencilRule lookup =
    let changes =
        allPositions
        |> List.map lookup
        |> List.map (fun c -> (c, cellPencils c))
        |> List.filter (fun (_, ds) -> ds.Count = 1)
        |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

    { rule = "single-pencil"
      changes = changes }

let updatePencilsRule lookup =
    let solveGroup group =
        let cellsInGroup = group |> List.map lookup

        let digitsInGroup =
            cellsInGroup
            |> List.map cellDigits
            |> List.fold Set.union Set.empty

        let pencilsToRemoveFromCell c =
            cellPencils c |> Set.intersect digitsInGroup

        cellsInGroup
        |> List.map (fun c -> c, pencilsToRemoveFromCell c)
        |> List.filter (fun (_, ds) -> ds.Count > 0)
        |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

    let changes = allGroups |> List.collect solveGroup

    { rule = "fix-pencils"
      changes = changes }

let mapDigitsToGroup group =
    let map = List.zip allDigits group |> Map.ofList
    (fun digit -> Map.find digit map)

let hiddenPencilsRule (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
    let len = combo.inside.Length

    let mapper = mapDigitsToGroup group

    let insideCells =
        combo.inside |> List.map mapper |> List.map lookup

    let outsideCells =
        combo.outside
        |> List.map mapper
        |> List.map lookup

    let insideDigits = cellGroupPencils insideCells
    let outsideDigits = cellGroupPencils outsideCells
    let uniqueDigits = insideDigits - outsideDigits

    let changes =
        if uniqueDigits.Count <> len
           || uniqueDigits = insideDigits then
            List.empty
        else
            insideCells
            |> List.map (fun c -> c.position, RetainPencils uniqueDigits)

    { rule = $"hidden-pencils-%d{len}"
      changes = changes }

let allHiddenPencilRules =
    List.allPairs allGroups AllDigitCombinations
    |> List.map (fun (group, combo) -> hiddenPencilsRule group combo)

let nakedPencilsRule (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
    let len = combo.inside.Length

    let mapper = mapDigitsToGroup group

    let insideCells =
        combo.inside |> List.map mapper |> List.map lookup

    let outsideCells =
        combo.outside
        |> List.map mapper
        |> List.map lookup

    let insideDigits = cellGroupPencils insideCells
    let outsideDigits = cellGroupPencils outsideCells
    let commonDigits = Set.intersect insideDigits outsideDigits

    let changes =
        if insideDigits.Count <> len
           || commonDigits.Count = 0 then
            List.empty
        else
            outsideCells
            |> List.filter (fun c -> cellContainsPencils c commonDigits)
            |> List.map (fun c -> c.position, RemovePencils commonDigits)

    { rule = $"naked-pencils-%d{len}"
      changes = changes }

let allNakedPencilRules =
    List.allPairs allGroups AllDigitCombinations
    |> List.map (fun (group, combo) -> nakedPencilsRule group combo)

let AllRules =
    [ updatePencilsRule ]
    @ [ singlePencilRule ]
    @ allNakedPencilRules @ allHiddenPencilRules

let solvePuzzle puzzle =
    let applyAllRulesToPuzzle pz = applyRules (cellFinder pz) AllRules

    let mutable steps =
        List.singleton { rule = "start"; puzzle = puzzle }

    let mutable puzzle2 = puzzle
    let mutable results = applyRules (cellFinder puzzle2) AllRules

    while results.changes.Length > 0 do
        puzzle2 <- applyRuleResults results.changes puzzle2

        let step =
            { rule = results.rule
              puzzle = puzzle2 }

        steps <- steps @ [ step ]
        results <- puzzle2 |> applyAllRulesToPuzzle

    (puzzle2, steps)
