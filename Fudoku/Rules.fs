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
            |> List.filter (fun (_, ds) -> ds.Count = 1)
            |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

        { rule = "single-pencil"; changes = changes }

    let rule lookup = singlePencilRule lookup AllPositions

module FixPencils =

    let private fixPencilsRule lookup =
        let solveGroup group =
            let cellsInGroup = List.map lookup group

            let digitsInGroup =
                cellsInGroup
                |> List.map cellDigit
                |> List.fold Set.union Set.empty

            let pencilsToRemoveFromCell c = Set.intersect digitsInGroup (cellPencils c)

            cellsInGroup
            |> List.map (fun c -> c, pencilsToRemoveFromCell c)
            |> List.filter (fun (_, ds) -> ds.Count > 0)
            |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

        let changes = List.collect solveGroup AllGroups

        { rule = "fix-pencils"; changes = changes }

    let rule lookup = fixPencilsRule lookup

module SingleCell =
    let private singleCellPencil (group: Position list) (combo: Combination<Digit>) (lookup: CellFinder) =
        let cells = lookupCellCombination group combo lookup

        let insideDigits = groupPencils cells.inside
        let outsideDigits = groupPencils cells.outside
        let uniqueDigits = insideDigits - outsideDigits

        if uniqueDigits.Count = 1 then
            cells.inside
            |> List.map (fun c -> c.position, Solved uniqueDigits.MinimumElement)
        else
            List.empty

    let rule lookup =
        let changes =
            List.allPairs AllGroups DigitSingles
            |> List.collect (fun (group, combo) -> singleCellPencil group combo lookup)

        { rule = "single-cell"; changes = changes }

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

                let availableDigits = Set.intersect currentDigits remainingDigits

                availableDigits
                |> Set.toList
                |> List.collect (fun d -> (cellsWithPencil d remainingCells))
                |> List.exists (fun (d, c) -> tryNextCell d c)

            if remainingCells.IsEmpty && remainingDigits.IsEmpty then true
            elif remainingCells.IsEmpty || remainingDigits.IsEmpty then false
            else tryRemainingCells

        List.exists (solveForCell cells digits) cells

    let hiddenPencils (group: Position list) (combo: Combination<Digit>) (lookup: CellFinder) =
        let cells = lookupCellCombination group combo lookup

        let len = cells.inside.Length
        let insideDigits = groupPencils cells.inside
        let outsideDigits = groupPencils cells.outside
        let uniqueDigits = insideDigits - outsideDigits

        let changes =
            if uniqueDigits.Count = len
               && uniqueDigits <> insideDigits
               && cellsLinkedByDigits cells.inside uniqueDigits then
                cells.inside
                |> List.map (fun c -> c.position, RetainPencils uniqueDigits)
            else
                List.empty

        { rule = $"hidden-pencils-%d{len}"; changes = changes }

    let nakedPencils (group: Position list) (combo: Combination<Digit>) (lookup: CellFinder) =
        let cells = lookupCellCombination group combo lookup

        let len = cells.inside.Length
        let insideDigits = groupPencils cells.inside
        let outsideDigits = groupPencils cells.outside
        let commonDigits = Set.intersect insideDigits outsideDigits

        let changes =
            if insideDigits.Count = len
               && commonDigits.Count > 0
               && cellsLinkedByDigits cells.inside insideDigits then
                cells.outside
                |> List.filter (cellContainsPencils commonDigits)
                |> List.map (fun c -> c.position, RemovePencils commonDigits)
            else
                List.empty

        { rule = $"naked-pencils-%d{len}"; changes = changes }

    let hiddenRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> hiddenPencils group combo)

    let nakedRules =
        List.allPairs AllGroups MultiDigitCombinations
        |> List.map (fun (group, combo) -> nakedPencils group combo)

module SingleBox =
    let private singleBoxRule (combo: Combination<Position>) (lookup: CellFinder) =
        let insideCells: Cell list = combo.inside |> List.map lookup

        let outsideCells: Cell list = combo.outside |> List.map lookup

        let insidePencils = groupPencils insideCells
        let outsidePencils = groupPencils outsideCells

        let uniquePencils = Set.difference insidePencils outsidePencils

        let isAllInBox = uniquePencils.Count > 0

        let boxCellsToChange () =
            let first = List.head insideCells

            let neighbors =
                boxNeighbors first.position
                |> List.except combo.inside
                |> List.map lookup
                |> List.filter (cellContainsPencils uniquePencils)

            neighbors
            |> List.map (fun c -> c.position, RemovePencils uniquePencils)

        let changes = if isAllInBox then boxCellsToChange () else List.empty

        { rule = "single-box"; changes = changes }

    let rules =
        let rowMappers =
            AllDigits
            |> List.map (fun r -> (fun c -> position r c))

        let colMappers =
            AllDigits
            |> List.map (fun c -> (fun r -> position r c))

        let allMappers = rowMappers @ colMappers

        List.allPairs allMappers SingleSegmentDigitTriples
        |> List.map (fun (mapper, combo) -> combinationMapper mapper combo)
        |> List.map (fun combo -> singleBoxRule combo)

module EvenPath =
    let cellPencilList cell =
        cellPencils cell
        |> Set.toList
        |> List.map (fun d -> (d, cell.position))

    let createDigitMap (lookup: CellFinder) : Map<Digit, Set<Position>> =
        AllPositions
        |> List.map lookup
        |> List.collect cellPencilList
        |> SetMap.ofPairs

    let jumpsFrom (source: Position) (positions: Set<Position>) : Set<Position * Position> =
        let jumpsInGroup group =
            let active = Set.ofList group |> Set.intersect positions

            if Set.count active = 1 then [ (source, Set.minElement active) ] else []

        [ (rowNeighbors source); (colNeighbors source); (boxNeighbors source) ]
        |> List.collect jumpsInGroup
        |> Set.ofList

    let allPossibleJumps (positionList: List<Position>) (positionSet: Set<Position>) =
        positionList
        |> List.map (fun p -> jumpsFrom p positionSet)
        |> List.fold (fun ps p -> Set.union p ps) Set.empty

    let solveForPosition (from: Position) (positionSet: Set<Position>) (validJumps: Set<Position * Position>) : Position option =
        let neighborsOf pos =
            allNeighbors pos
            |> List.filter (setContainsElement positionSet)

        let finishPath (path: Position list) = if (List.length path) % 2 = 0 then Some from else None

        let rec loop current neighbors (path: Position list) jumps =
            if current = from && path.Length > 0 then
                finishPath (current :: path)
            else
                match neighbors with
                | [] -> None
                | next :: remaining ->
                    if not (Set.contains (current, next) jumps) then
                        loop current remaining path jumps
                    else
                        let nextNeighbors = neighborsOf next
                        let nextPath = current :: path

                        let nextJumps =
                            jumps
                            |> Set.remove (current, next)
                            |> Set.remove (next, current)

                        loop next nextNeighbors nextPath nextJumps

        let neighbors = neighborsOf from
        loop from neighbors [] validJumps

    let solveForDigit digit positionSet =
        let positionList = positionSet |> Set.toList
        let jumpSet = allPossibleJumps positionList positionSet

        positionList
        |> List.map (fun p -> solveForPosition p positionSet jumpSet)
        |> List.tryFind Option.isSome
        |> Option.bind id
        |> Option.map (fun p -> p, RemovePencils(Set.singleton digit))
        |> Option.toList

    let rule lookup =
        let digitMap = createDigitMap lookup

        let changes =
            SetMap.keys digitMap
            |> List.map (fun digit -> solveForDigit digit (SetMap.get digit digitMap))
            |> List.tryFind (fun changes -> not (List.isEmpty changes))
            |> Option.defaultValue []

        { rule = "digit-chain"; changes = changes }
