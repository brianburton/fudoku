module Fudoku.Puzzle

open Fudoku.Utils
open Fudoku.Domain

type PuzzleSolution = FastMap<Position, Digit>
type Puzzle = private Puzzle of FastMap<Position, Cell>

type RuleResultChange =
    | Solved of Digit
    | RemovePencils of FastSet<Digit>
    | RetainPencils of FastSet<Digit>
    | AddPencils of FastSet<Digit>

type RuleResult = { rule: string; changes: (Position * RuleResultChange) list }

type Rule = CellFinder -> RuleResult

type CellDiff = { diffPosition: Position; before: CellValue; after: CellValue }

let emptyPuzzle: Puzzle =
    AllPositions
    |> List.map (fun p -> p, starterCell p)
    |> FastMap.ofSeq
    |> Puzzle

let addToPuzzle (pz: Puzzle) (cells: Cell list) : Puzzle =
    let addCell (Puzzle puzzle) cell = FastMap.add cell.position cell puzzle |> Puzzle

    cells |> List.fold addCell pz

let isCompleteSolution (s: PuzzleSolution) = (FastMap.length s) = AllPositions.Length

let cellFinder (Puzzle pz) = fun p -> FastMap.find p pz

let applyRuleResults results (Puzzle puzzle) =
    let solve p d pz = pz |> FastMap.add p (solvedCell p d)

    let updatePencils p ds pz op =
        let currentPencils = cellPencils (FastMap.find p pz)
        let changedPencils = op currentPencils ds
        let changedCell = unsolvedCell p changedPencils
        FastMap.add p changedCell pz

    let remove p ds pz = updatePencils p ds pz FastSet.difference

    let retain p ds pz = updatePencils p ds pz FastSet.intersect
    let add p ds pz = updatePencils p ds pz FastSet.union

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemovePencils ds -> remove p ds pz
        | RetainPencils ds -> retain p ds pz
        | AddPencils ds -> add p ds pz

    let applied = List.fold applyResult puzzle results

    Puzzle applied

let rec applyRules lookup rules =
    match rules with
    | [] -> { rule = ""; changes = List.empty }
    | rule :: tail ->
        let result = rule lookup

        if result.changes.Length > 0 then result else applyRules lookup tail

let puzzleFromList list = addToPuzzle emptyPuzzle list

let diffPuzzles pz1 pz2 =
    AllPositions
    |> List.map (fun p -> p, (pz1 p), (pz2 p))
    |> List.filter (fun (_, a, b) -> a <> b)
    |> List.map (fun (p, a, b) -> { diffPosition = p; before = a.value; after = b.value })

let positionsWithPencilsSet (lookup: CellFinder) =
    let hasPencils cell =
        match cell.value with
        | Pencils ds -> FastSet.length ds >= 2
        | _ -> false

    Seq.ofList AllPositions
    |> Seq.map lookup
    |> Seq.filter hasPencils
    |> Seq.map (fun c -> c.position)
    |> FastSet.ofSeq

let isSolved (Puzzle puzzle) =
    FastMap.toSeq puzzle
    |> Seq.map snd
    |> Seq.forall
        (fun cell ->
            match cell.value with
            | Answer _ -> true
            | _ -> false)
