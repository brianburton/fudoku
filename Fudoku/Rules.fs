namespace Fudoku

open Domain
open Puzzle
open Utils

module SingleDigit =
    let private singlePencilRule lookup group =
        let changes =
            group
            |> List.map lookup
            |> List.map (fun c -> (c, cellPencils c))
            |> List.filter (fun (_, ds) -> (FastSet.length ds) = 1)
            |> List.map (fun (c, ds) -> c.position, Solved(FastSet.head ds))

        { rule = "single-pencil"; changes = changes }

    let rule lookup = singlePencilRule lookup AllPositions

module FixPencils =

    let fixPencilsRule lookup =
        let solveGroup group =
            let cellsInGroup = List.map lookup group

            let digitsInGroup =
                cellsInGroup
                |> List.map cellDigit
                |> List.fold FastSet.union NoDigits

            let pencilsToRemoveFromCell c = FastSet.intersect digitsInGroup (cellPencils c)

            cellsInGroup
            |> List.map (fun c -> c, pencilsToRemoveFromCell c)
            |> List.filter (fun (_, ds) -> (FastSet.length ds) > 0)
            |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

        let changes = List.collect solveGroup AllGroups

        { rule = "fix-pencils"; changes = changes }

module SingleCell =
    let private singleCellPencil (lookup: CellFinder) (group: Position list) =
        let map = createDigitMap group lookup

        SetMap.toSeq map
        |> Seq.filter (fun (_, ps) -> FastSet.length ps = 1)
        |> Seq.map (fun (d, ps) -> FastSet.head ps, Solved d)
        |> Seq.toList

    let rule lookup =
        let changes =
            AllGroups
            |> List.collect (singleCellPencil lookup)

        { rule = "single-cell"; changes = changes }

module Tuple =

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




module SingleBox =
    let private Combos =
        let rowMappers =
            AllDigits
            |> List.map (fun r -> (fun c -> position r c))

        let colMappers =
            AllDigits
            |> List.map (fun c -> (fun r -> position r c))

        let allMappers = rowMappers @ colMappers

        List.allPairs allMappers SingleSegmentDigitTriples
        |> List.map (fun (mapper, combo) -> combinationMapper mapper combo)

    let private solveSingleBox (combo: Combination<Position>) (lookup: CellFinder) =
        let insideCells: Cell list = combo.inside |> List.map lookup

        let outsideCells: Cell list = combo.outside |> List.map lookup

        let insidePencils = groupPencils insideCells
        let outsidePencils = groupPencils outsideCells

        let uniquePencils = FastSet.difference insidePencils outsidePencils

        let isAllInBox = FastSet.length uniquePencils > 0

        let boxCellsToChange () =
            let first = List.head insideCells

            let neighbors =
                boxNeighbors first.position
                |> List.except combo.inside
                |> List.map lookup
                |> List.filter (cellContainsPencils uniquePencils)

            neighbors
            |> List.map (fun c -> c.position, RemovePencils uniquePencils)

        if isAllInBox then boxCellsToChange () else List.empty

    let rule (lookup: CellFinder) =
        let changes =
            Seq.ofList Combos
            |> Seq.map (fun c -> solveSingleBox c lookup)
            |> Seq.tryFind (fun changes -> not (List.isEmpty changes))
            |> Option.defaultValue []

        { rule = "single-box"; changes = changes }

module BUG =
    let seekBUG (lookup: CellFinder) : Cell option =
        let summary =
            AllPositions
            |> List.map lookup
            |> List.map (fun cell -> (FastSet.length (cellPencils cell), cell))
            |> List.fold
                (fun (pairs, triples, others, triple) (count, cell) ->
                    match count with
                    | 0 -> (pairs, triples, others, triple)
                    | 2 -> (pairs + 1, triples, others, triple)
                    | 3 -> (pairs, triples + 1, others, Some cell)
                    | _ -> (pairs, triples, others + 1, triple))
                (0, 0, 0, None)

        match summary with
        | pairs, _, _, _ when pairs = 0 -> None
        | _, _, others, _ when others <> 0 -> None
        | _, triples, _, _ when triples <> 1 -> None
        | _, _, _, triple -> triple

    let solveBUG (lookup: CellFinder) (bug: Cell) : Position * RuleResultChange =
        let rowMap = createDigitMap ((rowNeighbors bug.position) @ [ bug.position ]) lookup

        let threeDigit =
            SetMap.keys rowMap
            |> List.map (fun digit -> digit, SetMap.getCount digit rowMap)
            |> List.find (fun (_, count) -> count = 3)
            |> fst

        bug.position, Solved threeDigit

    let rule lookup =
        let changes =
            seekBUG lookup
            |> Option.map (solveBUG lookup)
            |> Option.toList

        { rule = "BUG"; changes = changes }
