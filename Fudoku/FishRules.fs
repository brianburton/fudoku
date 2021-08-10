module Fudoku.FishRules

open Fudoku.Domain
open Fudoku.Utils
open Fudoku.Puzzle

type FishDirection =
    | RowFish
    | ColFish

let lookupFishCells (rowCombo: DigitCombination) (colCombo: DigitCombination) (lookup: CellFinder) direction =
    let inside =
        List.allPairs rowCombo.inside colCombo.inside
        |> List.map (fun (r, c) -> position r c)
        |> List.map lookup

    let outside =
        match direction with
        | RowFish -> List.allPairs rowCombo.inside colCombo.outside
        | ColFish -> List.allPairs rowCombo.outside colCombo.inside
        |> List.map (fun (r, c) -> position r c)
        |> List.map lookup

    { CellCombination.inside = inside
      outside = outside }

let lookupFishAffected (rowCombo: DigitCombination) (colCombo: DigitCombination) (lookup: CellFinder) direction =
    match direction with
    | RowFish -> List.allPairs rowCombo.outside colCombo.inside
    | ColFish -> List.allPairs rowCombo.inside colCombo.outside
    |> List.map (fun (r, c) -> position r c)
    |> List.map lookup

let rec allPositionsLinked (positions: Position list) : bool =

    let sameRowOrCol current next =
        current.row = next.row || current.col = next.col

    let rec solveForPos (remainingPositions: Position list) (current: Position) =
        let nextPos =
            findAndRemove remainingPositions (sameRowOrCol current)

        match nextPos with
        | None -> remainingPositions.IsEmpty
        | Some (pos, remaining) -> solveForPos remaining pos

    positions |> List.exists (solveForPos positions)

let atLeastTwoPerRowCol (group: Position list) =
    let increment key map =
        let current = Map.tryFind key map

        let next =
            match current with
            | Some count -> count + 1
            | None -> 1

        Map.add key next map

    let addCell map pos =
        increment ("row", pos.row) map
        |> increment ("col", pos.col)

    let countsMap = group |> List.fold addCell Map.empty

    Map.toList countsMap
    |> List.forall (fun (_, count) -> count >= 2)

let allArePresent positions expected mapper =
    let digits =
        positions |> List.map mapper |> Set.ofList

    let expectedSet = Set.ofList expected
    digits = expectedSet

let isValidFish positions rows cols =
    allArePresent positions rows (fun p -> p.row)
    && allArePresent positions cols (fun p -> p.col)
    && atLeastTwoPerRowCol positions
    && (allPositionsLinked positions)

let fishRuleForCombo (rowCombo: DigitCombination) (colCombo: DigitCombination) (lookup: CellFinder) direction =
    let combo =
        lookupFishCells rowCombo colCombo lookup direction

    let insidePencils = groupPencils combo.inside
    let outsidePencils = groupPencils combo.outside

    let uniquePencils =
        Set.difference insidePencils outsidePencils

    if uniquePencils.Count <> 1 then
        []
    else
        let positions =
            combo.inside
            |> List.filter (cellContainsPencils uniquePencils)
            |> List.map (fun c -> c.position)

        if isValidFish positions rowCombo.inside colCombo.inside then
            let affected =
                lookupFishAffected rowCombo colCombo lookup direction

            affected
            |> List.filter (cellContainsPencils uniquePencils)
            |> List.map (fun c -> (c.position, RemovePencils uniquePencils))
        else
            []
