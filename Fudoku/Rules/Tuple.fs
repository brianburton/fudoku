module Fudoku.Tuple

open Domain
open Puzzle

let cellsLinkedByDigits (cells: Cell list) (digits: FastSet<Digit>) : bool =
    let cellsWithPencil digit cells =
        cells
        |> List.filter (fun c -> FastSet.contains digit (cellPencils c))
        |> List.map (fun c -> digit, c)

    let rec solveForCell (remainingCells: Cell list) (remainingDigits: FastSet<Digit>) (current: Cell) =
        let tryNextCell digit cell =
            let newRemainingCells = List.except [ cell ] remainingCells
            let newRemainingDigits = FastSet.remove digit remainingDigits
            solveForCell newRemainingCells newRemainingDigits cell

        let tryRemainingCells =
            let currentDigits = (cellPencils current)

            let availableDigits = FastSet.intersect currentDigits remainingDigits

            availableDigits
            |> FastSet.toList
            |> List.collect (fun d -> (cellsWithPencil d remainingCells))
            |> List.exists (fun (d, c) -> tryNextCell d c)

        if remainingCells.IsEmpty
           && (FastSet.isEmpty remainingDigits) then
            true
        elif remainingCells.IsEmpty
             || (FastSet.isEmpty remainingDigits) then
            false
        else
            tryRemainingCells

    List.exists (solveForCell cells digits) cells

let private solveNakedPencils (cells: Combination<Cell>) =
    let len = cells.inside.Length
    let insideDigits = groupPencils cells.inside
    let outsideDigits = groupPencils cells.outside
    let commonDigits = FastSet.intersect insideDigits outsideDigits

    if (FastSet.length insideDigits) = len
       && (FastSet.length commonDigits) > 0
       && cellsLinkedByDigits cells.inside insideDigits then
        cells.outside
        |> List.filter (cellContainsPencils commonDigits)
        |> List.map (fun c -> c.position, RemovePencils commonDigits)
    else
        List.empty

let private solveHiddenPencils (cells: Combination<Cell>) =
    let len = cells.inside.Length
    let insideDigits = groupPencils cells.inside
    let outsideDigits = groupPencils cells.outside
    let uniqueDigits = FastSet.difference insideDigits outsideDigits

    if (FastSet.length uniqueDigits) = len
       && not (FastSet.equals uniqueDigits insideDigits)
       && cellsLinkedByDigits cells.inside uniqueDigits then
        cells.inside
        |> List.map (fun c -> c.position, RetainPencils uniqueDigits)
    else
        List.empty

let private allTupleCombinationsOfLength len (lookup: CellFinder) =
    AllGroups
    |> List.collect (findTupleCombinations len lookup)
    |> Seq.ofList

let private ruleTemplate solver title finder lookup =
    let changes =
        finder lookup
        |> Seq.map solver
        |> Seq.tryFind (fun changes -> not (List.isEmpty changes))
        |> Option.defaultValue []

    { rule = title; changes = changes }

let hiddenPairsRule = ruleTemplate solveHiddenPencils "hidden-pairs" (allTupleCombinationsOfLength 2)

let hiddenTriplesRule =
    ruleTemplate solveHiddenPencils "hidden-triples" (allTupleCombinationsOfLength 3)

let hiddenQuadsRule = ruleTemplate solveHiddenPencils "hidden-quads" (allTupleCombinationsOfLength 4)
let nakedPairsRule = ruleTemplate solveNakedPencils "naked-pairs" (allTupleCombinationsOfLength 2)
let nakedTriplesRule = ruleTemplate solveNakedPencils "naked-triples" (allTupleCombinationsOfLength 3)
let nakedQuadsRule = ruleTemplate solveNakedPencils "naked-quads" (allTupleCombinationsOfLength 4)
