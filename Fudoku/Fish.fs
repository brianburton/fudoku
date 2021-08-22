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

let lookupFishPositions (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) direction =
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

let lookupFishCells2 (lookup: CellFinder) (fish: Fish<Position>) =
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

let rec allPositionsLinked (positions: Position list) : bool =

    let rec solveForPos (skip: Set<Position>) (path: Position list) (current: Position) =
        let newPath = path @ [ current ]
        let newSkip = Set.add current skip

        if newSkip.Count = positions.Length then
            true
        else
            let rowNeighbors =
                positions
                |> List.filter (fun p -> p.row = current.row)
                |> adjacentPositionsInGroup newSkip current

            let colNeighbors =
                positions
                |> List.filter (fun p -> p.col = current.col)
                |> adjacentPositionsInGroup newSkip current

            List.append rowNeighbors colNeighbors
            |> List.exists (solveForPos newSkip newPath)

    positions
    |> List.forall (solveForPos Set.empty [])

let allArePresent positions expected mapper =
    let digits = positions |> List.map mapper |> Set.ofList

    let expectedSet = Set.ofList expected
    digits = expectedSet

let isValidFish positions rows cols =
    allArePresent positions rows (fun p -> p.row)
    && allArePresent positions cols (fun p -> p.col)
    && allPositionsLinked positions

let private makeFishCombos combos =
    List.allPairs combos combos
    |> List.allPairs [ RowFish; ColFish ]
    |> List.map (fun (direction, (rows, cols)) -> lookupFishPositions rows cols direction)

let private XWingPositions = makeFishCombos DigitPairs

let private SwordfishPositions = makeFishCombos DigitTriples

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

let private fishRuleTemplate (combos: Fish<Position> list) solver (title: string) (lookup: CellFinder) =
    let positionFilter = positionsWithPencilsSet lookup

    let changes =
        Seq.ofList combos
        |> Seq.filter (fun c -> FastSet.containsAll positionFilter c.fishInside)
        |> Seq.map (lookupFishCells2 lookup)
        |> Seq.map solver
        |> Seq.tryFind (fun changes -> not (List.isEmpty changes))
        |> Option.defaultValue []

    { rule = title; changes = changes }

let xWingRule = fishRuleTemplate XWingPositions solveFish "x-wing"
let swordfishRule = fishRuleTemplate SwordfishPositions solveFish "swordfish"
