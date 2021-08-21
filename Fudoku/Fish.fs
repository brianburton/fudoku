module Fudoku.Fish

open Fudoku.Domain
open Fudoku.Utils
open Fudoku.Puzzle

type FishDirection =
    | RowFish
    | ColFish

let lookupFishCells (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) (lookup: CellFinder) direction =
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

    { inside = inside
      outside = outside }

let lookupFishAffected (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) (lookup: CellFinder) direction =
    match direction with
    | RowFish -> List.allPairs rowCombo.outside colCombo.inside
    | ColFish -> List.allPairs rowCombo.inside colCombo.outside
    |> List.map (fun (r, c) -> position r c)
    |> List.map lookup

let adjacentPositionsInGroup (skip: Set<Position>) (center: Position) (group: Position list) : Position list =
    let add x found =
        if (Set.contains x skip) then
            found
        else
            found @ [ x ]

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
    let digits =
        positions |> List.map mapper |> Set.ofList

    let expectedSet = Set.ofList expected
    digits = expectedSet

let isValidFish positions rows cols =
    allArePresent positions rows (fun p -> p.row)
    && allArePresent positions cols (fun p -> p.col)
    && allPositionsLinked positions

let fishRuleForCombo (rowCombo: Combination<Digit>) (colCombo: Combination<Digit>) direction (lookup: CellFinder) =
    let combo =
        lookupFishCells rowCombo colCombo lookup direction

    let insidePencils = groupPencils combo.inside
    let outsidePencils = groupPencils combo.outside

    let uniquePencils =
        FastSet.difference insidePencils outsidePencils

    if (FastSet.length uniquePencils) <> 1 then
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

let makeFishRule combos name =
    List.allPairs combos combos
    |> List.allPairs [ RowFish; ColFish ]
    |> List.map (fun (direction, (rows, cols)) -> fishRuleForCombo rows cols direction)
    |> List.map (fun f -> (fun lookup -> { rule = name; changes = (f lookup) }))

let XWingRules: Rule list = makeFishRule DigitPairs "x-wing"

let SwordfishRules: Rule list = makeFishRule DigitTriples "swordfish"
