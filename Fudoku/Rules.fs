namespace Fudoku

open Domain
open Puzzle

module SingleDigit =
    let private singlePencilRule lookup group =
        let changes =
            group
            |> List.map lookup
            |> List.map (fun c -> (c, cellPencils c))
            |> List.filter (fun (_, ds) -> ds.Count = 1)
            |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

        { rule = "single-pencil"
          changes = changes }

    let rule lookup = singlePencilRule lookup AllPositions

module FixPencils =

    let private fixPencilsRule lookup =
        let solveGroup group =
            let cellsInGroup = List.map lookup group

            let digitsInGroup =
                cellsInGroup
                |> List.map cellDigit
                |> List.fold Set.union Set.empty

            let pencilsToRemoveFromCell c =
                Set.intersect digitsInGroup (cellPencils c)

            cellsInGroup
            |> List.map (fun c -> c, pencilsToRemoveFromCell c)
            |> List.filter (fun (_, ds) -> ds.Count > 0)
            |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

        let changes = List.collect solveGroup AllGroups

        { rule = "fix-pencils"
          changes = changes }

    let rule lookup = fixPencilsRule lookup

module Tuple =

    let cellsLinkedByDigits (cells: Cell list) (digits: Set<Digit>) : bool =
        let cellsWithPencil digit cells =
            cells
            |> List.filter (fun c -> Set.contains digit (cellPencils c))
            |> List.map (fun c -> digit, c)

        let rec solveForCell (remainingCells: Cell list) (remainingDigits: Set<Digit>) (current: Cell) =
            let tryNextCell digit cell =
                let newRemainingCells = List.except [ cell ] remainingCells
                let newRemainingDigits = Set.remove digit remainingDigits
                solveForCell newRemainingCells newRemainingDigits cell

            let tryRemainingCells =
                let currentDigits = (cellPencils current)

                let availableDigits =
                    Set.intersect currentDigits remainingDigits

                availableDigits
                |> Set.toList
                |> List.collect (fun d -> (cellsWithPencil d remainingCells))
                |> List.exists (fun (d, c) -> tryNextCell d c)

            if remainingCells.IsEmpty && remainingDigits.IsEmpty then
                true
            elif remainingCells.IsEmpty || remainingDigits.IsEmpty then
                false
            else
                tryRemainingCells

        List.exists (solveForCell cells digits) cells

    let private summarize (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
        let len = combo.inside.Length

        let mapper =
            let map = List.zip AllDigits group |> Map.ofList
            (fun digit -> Map.find digit map)

        let insideCells =
            combo.inside |> List.map mapper |> List.map lookup

        let outsideCells =
            combo.outside
            |> List.map mapper
            |> List.map lookup

        (len, insideCells, outsideCells)

    let singleCellPencil (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
        let len, insideCells, outsideCells = summarize group combo lookup

        let insideDigits = groupPencils insideCells
        let outsideDigits = groupPencils outsideCells
        let uniqueDigits = insideDigits - outsideDigits

        let changes =
            if len = 1 && uniqueDigits.Count = len then
                insideCells
                |> List.map (fun c -> c.position, Solved uniqueDigits.MinimumElement)
            else
                List.empty

        { rule = $"single-cell-pencil"
          changes = changes }

    let hiddenPencils (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
        let len, insideCells, outsideCells = summarize group combo lookup

        let insideDigits = groupPencils insideCells
        let outsideDigits = groupPencils outsideCells
        let uniqueDigits = insideDigits - outsideDigits

        let changes =
            if uniqueDigits.Count = len
               && uniqueDigits <> insideDigits
               && cellsLinkedByDigits insideCells uniqueDigits then
                insideCells
                |> List.map (fun c -> c.position, RetainPencils uniqueDigits)
            else
                List.empty

        { rule = $"hidden-pencils-%d{len}"
          changes = changes }

    let nakedPencils (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
        let len, insideCells, outsideCells = summarize group combo lookup

        let insideDigits = groupPencils insideCells
        let outsideDigits = groupPencils outsideCells
        let commonDigits = Set.intersect insideDigits outsideDigits

        let changes =
            if insideDigits.Count = len
               && commonDigits.Count > 0
               && cellsLinkedByDigits insideCells insideDigits then
                outsideCells
                |> List.filter (fun c -> cellContainsPencils c commonDigits)
                |> List.map (fun c -> c.position, RemovePencils commonDigits)
            else
                List.empty

        { rule = $"naked-pencils-%d{len}"
          changes = changes }

    let singleCellRules =
        List.allPairs AllGroups DigitSingles
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let hiddenRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let nakedRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> nakedPencils group combo)

module SingleBox =
    let private singleBoxRule combo (lookup:CellFinder) =
        let insideCells : Cell list =
            combo.inside |> List.map lookup

        let outsideCells : Cell list =
            combo.outside
            |> List.map lookup

        let insidePencils = groupPencils insideCells
        let outsidePencils = groupPencils outsideCells
        let commonPencils = Set.intersect insidePencils outsidePencils
        let isAllInBox = commonPencils.Count = 0

        let boxCellsToChange () =
            let first = List.head insideCells
            let neighbors = boxNeighbors first.position
                            |> List.except combo.inside
                            |> List.map lookup
                            |> List.filter (fun c -> cellContainsPencils c insidePencils)
            neighbors
            |> List.map (fun c -> c.position, RemovePencils insidePencils)

        let changes =
            if isAllInBox then boxCellsToChange () else List.empty

        { rule = "single-box"
          changes = changes }

    let rules =
        let rowMappers =
            AllDigits
            |> List.map (fun r -> (fun c -> position r c))
        let colMappers =
            AllDigits
            |> List.map (fun c -> (fun r -> position r c))
        let allMappers = rowMappers @ colMappers
        List.allPairs allMappers SingleSegmentDigitTriples
        |> List.map (fun (mapper,combo)-> combinationMapper mapper combo)
        |> List.map (fun combo -> singleBoxRule combo)
