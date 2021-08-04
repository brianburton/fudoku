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

    let private atLeastTwoDigitsInAll (cells: Cell list) (digits: Set<Digit>) =
        let atLeastTwoInCommon cell =
            let cellDigits = cellPencils cell
            let common = Set.intersect digits cellDigits
            common.Count >= 2

        cells
        |> List.map atLeastTwoInCommon
        |> List.fold (fun a b -> a && b) true

    let hiddenPencils (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
        let len, insideCells, outsideCells = summarize group combo lookup

        let insideDigits = groupPencils insideCells
        let outsideDigits = groupPencils outsideCells
        let uniqueDigits = insideDigits - outsideDigits

        let changes =
            if uniqueDigits.Count = len
               && uniqueDigits <> insideDigits
               && atLeastTwoDigitsInAll insideCells uniqueDigits then
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
               && atLeastTwoDigitsInAll insideCells insideDigits then
                outsideCells
                |> List.filter (fun c -> cellContainsPencils c commonDigits)
                |> List.map (fun c -> c.position, RemovePencils commonDigits)
            else
                List.empty

        { rule = $"naked-pencils-%d{len}"
          changes = changes }

    let hiddenRules =
        List.allPairs AllGroups AllDigitCombinations
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let nakedRules =
        List.allPairs AllGroups AllDigitCombinations
        |> List.map (fun (group, combo) -> nakedPencils group combo)
