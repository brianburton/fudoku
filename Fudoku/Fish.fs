module Fudoku.Fish

open Fudoku.Domain
open Fudoku.Utils
open Fudoku.Puzzle

type FishDirection =
    | RowFish
    | ColFish

type Fish<'a> =
    { fishRows: Digit list
      fishCols: Digit list
      fishInside: 'a list
      fishOutside: 'a list
      fishAffected: 'a list }

let jumpOrder (a, b) = if b >= a then (a, b) else (b, a)

let rowSorted (positions: Position seq) = positions |> Seq.sort

let colSorted (positions: Position seq) =
    positions
    |> Seq.sortBy (fun p -> { row = p.col; col = p.row })

let allPossibleJumps (positionList: List<Position>) =
    let rowJumps =
        rowSorted positionList
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> a.row = b.row)

    let colJumps =
        colSorted positionList
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> a.col = b.col)

    Seq.append rowJumps colJumps
    |> Seq.collect (fun (a, b) -> [ (a, b); (b, a) ])
    |> SetMap.ofPairs

let createPositionFish (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) direction =
    let inside =
        List.allPairs rowCombo.inside colCombo.inside
        |> List.map (fun (r, c) -> position r c)

    let outside =
        match direction with
        | RowFish -> List.allPairs rowCombo.inside colCombo.outside
        | ColFish -> List.allPairs rowCombo.outside colCombo.inside
        |> List.map (fun (r, c) -> position r c)

    let affected =
        match direction with
        | RowFish -> List.allPairs rowCombo.outside colCombo.inside
        | ColFish -> List.allPairs rowCombo.inside colCombo.outside
        |> List.map (fun (r, c) -> position r c)

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

let adjacentPositionsInGroup (skip: Set<Position>) (center: Position) (group: Position list) : Position list =
    let add x found = if (Set.contains x skip) then found else found @ [ x ]

    let rec loop remaining found =
        match remaining with
        | [] -> found
        | [ _ ] -> found
        | x :: y :: _ when x = center -> add y found
        | x :: [ y ] when y = center -> add x found
        | x :: y :: z :: _ when y = center -> found |> add x |> add z
        | _ :: tail -> loop tail found

    loop group []

let allPositionsLinked (positions: Position list) : bool =
    let validJumps = allPossibleJumps positions

    let rec solveForPos (remaining: FastSet<Position>) (path: Position list) (current: Position) =
        let newPath = path @ [ current ]

        let newRemaining = FastSet.remove current remaining

        if FastSet.isEmpty newRemaining then
            true
        else
            let neighbors =
                validJumps
                |> SetMap.get current
                |> FastSet.intersect newRemaining

            neighbors
            |> FastSet.toSeq
            |> Seq.exists (solveForPos newRemaining newPath)

    let startRemaining = positions |> FastSet.ofSeq

    positions
    |> List.forall (solveForPos startRemaining  [])

let allArePresent positions expected mapper =
    let digits = positions |> List.map mapper |> Set.ofList

    let expectedSet = Set.ofList expected
    digits = expectedSet

let isValidFish positions rows cols =
    allArePresent positions rows (fun p -> p.row)
    && allArePresent positions cols (fun p -> p.col)
    //&& allPositionsLinked positions

let private possibleDimensionsForGroup len (lookup: CellFinder) (group: Position list) =
    let positions = findTuplePositions len lookup group
    let rows = positions |> FastSet.map (fun p -> p.row)
    let cols = positions |> FastSet.map (fun p -> p.col)
    rows, cols

let private possibleDimensionsForGroups (groups: Position list list) len (lookup: CellFinder) =
    groups
    |> Seq.ofList
    |> Seq.map (possibleDimensionsForGroup len lookup)
    |> Seq.fold (fun (ra, ca) (r, c) -> (FastSet.union ra r), (FastSet.union ca c)) (NoDigits, NoDigits)

let private possibleFishForDirection direction (groups: Position list list) len (lookup: CellFinder) =
    let rows, cols = possibleDimensionsForGroups groups len lookup

    let rowCombos =
        combinations len (FastSet.toList rows)
        |> List.map comboOf

    let colCombos =
        combinations len (FastSet.toList cols)
        |> List.map comboOf

    Seq.allPairs rowCombos colCombos
    |> Seq.map (fun (rs, cs) -> createPositionFish rs cs direction)
    |> Seq.map (convertToCellFish lookup)

let private allPossibleFish len (lookup: CellFinder) =
    seq {
        yield! possibleFishForDirection RowFish AllRows len lookup
        yield! possibleFishForDirection ColFish AllCols len lookup
    }

let private solveFish (cells: Fish<Cell>) =

    let insidePencils = cells.fishInside |> groupPencils

    let outsidePencils = cells.fishOutside |> groupPencils

    let uniquePencils = FastSet.difference insidePencils outsidePencils

    if (FastSet.length uniquePencils) <> 1 then
        []
    else
        let positions =
            cells.fishInside
            |> List.filter (cellContainsPencils uniquePencils)
            |> List.map (fun c -> c.position)

        if isValidFish positions cells.fishRows cells.fishCols then
            cells.fishAffected
            |> List.filter (cellContainsPencils uniquePencils)
            |> List.map (fun c -> (c.position, RemovePencils uniquePencils))
        else
            []

let private ruleTemplate (title: string) (fishFinder: CellFinder -> Fish<Cell> seq) (lookup: CellFinder) =
    let changes =
        fishFinder lookup
        |> Seq.map solveFish
        |> Seq.tryFind (fun changes -> not (List.isEmpty changes))
        |> Option.defaultValue []

    { rule = title; changes = changes }

let xWingRule = ruleTemplate "x-wing" (allPossibleFish 2)
let swordfishRule = ruleTemplate "swordfish" (allPossibleFish 3)
