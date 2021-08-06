namespace Fudoku

open Domain
open Puzzle

module SingleDigit =
    let singlePencilRule lookup group =
        let changes =
            group
            |> List.map lookup
            |> List.map (fun c -> (c, cellPencils c))
            |> List.filter (fun (_, ds) -> ds.Count = 1)
            |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

        { rule = "single-pencil"
          changes = changes }

    let rule lookup = singlePencilRule lookup AllPositions

module Tuple =

    let cellsLinkedByDigits (cells: Cell list) (digits: Set<Digit>) : bool =
        let cellsWithPencil digit cells =
            cells
            |> List.filter (fun c -> Set.contains digit (cellPencils c))
            |> List.map (fun c -> digit, c)

        let rec next (remainingCells: Cell list) (remainingDigits: Set<Digit>) (current: Cell) =
            let impl digit cell =
                let newRemainingCells = List.except [ cell ] remainingCells
                let newRemainingDigits = Set.remove digit remainingDigits
                next newRemainingCells newRemainingDigits cell

            if remainingCells.IsEmpty && remainingDigits.IsEmpty then
                true
            elif remainingCells.IsEmpty || remainingDigits.IsEmpty then
                false
            else
                let currentDigits = (cellPencils current)

                let availableDigits =
                    Set.intersect currentDigits remainingDigits

                availableDigits
                |> Set.toList
                |> List.collect (fun d -> (cellsWithPencil d remainingCells))
                |> List.exists (fun (d, c) -> impl d c)

        cells |> List.exists (next cells digits)

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

    let singleCell =
        List.allPairs AllGroups DigitSingles
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let hiddenRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let nakedRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> nakedPencils group combo)
