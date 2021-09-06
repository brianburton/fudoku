module Fudoku.Fish

open Domain
open Utils
open Puzzle

type FishDirection =
    | RowFish
    | ColFish

type Fish<'a> =
    { fishRows: Digit list
      fishCols: Digit list
      fishInside: 'a list
      fishOutside: 'a list
      fishAffected: 'a list }

let createPositionFish (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) direction =
    let inside =
        List.allPairs rowCombo.inside colCombo.inside
        |> List.map pairToPosition

    let outside =
        match direction with
        | RowFish -> List.allPairs rowCombo.inside colCombo.outside
        | ColFish -> List.allPairs rowCombo.outside colCombo.inside
        |> List.map pairToPosition

    let affected =
        match direction with
        | RowFish -> List.allPairs rowCombo.outside colCombo.inside
        | ColFish -> List.allPairs rowCombo.inside colCombo.outside
        |> List.map pairToPosition

    { fishRows = rowCombo.inside
      fishCols = colCombo.inside
      fishInside = inside
      fishOutside = outside
      fishAffected = affected }

let convertToCellFish (lookup: CellFinder) (fish: Fish<Position>) =
    { fishRows = fish.fishRows
      fishCols = fish.fishCols
      fishInside = fish.fishInside |> List.map lookup
      fishOutside = fish.fishOutside |> List.map lookup
      fishAffected = fish.fishAffected |> List.map lookup }

let allArePresent positions expected mapper =
    let digits = positions |> List.map mapper |> FastSet.ofSeq
    let expectedSet = FastSet.ofSeq expected
    digits = expectedSet

let isValidFish positions rows cols =
    allArePresent positions rows (fun p -> p.row)
    && allArePresent positions cols (fun p -> p.col)

let private solveFish (cells: Fish<Cell>) =

    let insidePencils = cells.fishInside |> groupPencils

    let outsidePencils = cells.fishOutside |> groupPencils

    let uniquePencils = FastSet.difference insidePencils outsidePencils

    let rec loop digits =
        match digits with
        | [] -> []
        | digit :: tail ->
            let digitSet = (FastSet.singleton digit)

            let positions =
                cells.fishInside
                |> List.filter (cellContainsPencil digit)
                |> List.map (fun c -> c.position)

            let changes =
                if isValidFish positions cells.fishRows cells.fishCols then
                    cells.fishAffected
                    |> List.filter (cellContainsPencil digit)
                    |> List.map (fun c -> (c.position, RemovePencils digitSet))
                else
                    []

            if changes.Length > 0 then changes else loop tail

    loop (FastSet.toList uniquePencils)

let private ruleTemplate (title: string) (fishFinder: CellFinder -> Fish<Cell> seq) (lookup: CellFinder) =
    let changes =
        fishFinder lookup
        |> Seq.map solveFish
        |> firstNonEmptyList

    { rule = title; changes = changes }

let findCandidates len posMap =
    let ysForXs xs =
        xs
        |> List.map (swapArgs SetMap.get posMap)
        |> List.fold FastSet.union NoDigits
        |> FastSet.toList

    let allXs =
        SetMap.toSeq posMap
        |> Seq.filter (fun (_, ys) -> validTupleSet len ys)
        |> Seq.map fst
        |> List.ofSeq

    let combosOfXs = combinations len allXs

    combosOfXs
    |> List.map (fun xs -> (xs, (ysForXs xs)))
    |> List.filter (fun (_, ys) -> validTupleList len ys)
    |> List.toSeq

let private createRowFish len positions =
    let allRowsMap =
        FastSet.toSeq positions
        |> Seq.map (fun pos -> (pos.row, pos.col))
        |> SetMap.ofPairs

    let allRowFish = findCandidates len allRowsMap

    allRowFish
    |> Seq.map
        (fun (rows, cols) ->
            let rowCombo = groupCombo AllDigits rows
            let colCombo = groupCombo AllDigits cols
            createPositionFish rowCombo colCombo RowFish)

let private createColFish len positions =
    let allColsMap =
        FastSet.toSeq positions
        |> Seq.map (fun pos -> (pos.col, pos.row))
        |> SetMap.ofPairs

    let allColFish = findCandidates len allColsMap

    allColFish
    |> Seq.map
        (fun (cols, rows) ->
            let rowCombo = groupCombo AllDigits rows
            let colCombo = groupCombo AllDigits cols
            createPositionFish rowCombo colCombo ColFish)

let findAllCandidates len allDigitsMap lookup =
    SetMap.toSeq allDigitsMap
    |> Seq.collect
        (fun (_, positions) ->
            seq {
                yield! createRowFish len positions
                yield! createColFish len positions
            }
            |> Seq.map (convertToCellFish lookup))

let private allPossibleFish len lookup =
    let allDigitsMap = createDigitMap AllPositions lookup
    let candidates = findAllCandidates len allDigitsMap lookup
    candidates

let xWingRule = ruleTemplate "x-wing" (allPossibleFish 2)
let swordfishRule = ruleTemplate "swordfish" (allPossibleFish 3)
let jellyfishRule = ruleTemplate "jellyfish" (allPossibleFish 4)
