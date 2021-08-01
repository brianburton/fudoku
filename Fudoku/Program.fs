// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let memoize fn =
    let cache =
        System.Collections.Generic.Dictionary<_, _>()

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = fn (x)
            cache.Add(x, v)
            v)

type Digit =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine

let allDigits =
    [ One
      Two
      Three
      Four
      Five
      Six
      Seven
      Eight
      Nine ]

let allDigitsSet = allDigits |> Set.ofList

type Segment =
    | SegOne
    | SegTwo
    | SegThree

let segment d =
    match d with
    | One -> SegOne
    | Two -> SegOne
    | Three -> SegOne
    | Four -> SegTwo
    | Five -> SegTwo
    | Six -> SegTwo
    | Seven -> SegThree
    | Eight -> SegThree
    | Nine -> SegThree

let segDigits s =
    match s with
    | SegOne -> [ One; Two; Three ]
    | SegTwo -> [ Four; Five; Six ]
    | SegThree -> [ Seven; Eight; Nine ]

type Pos = { row: Digit; col: Digit }

let pos r c = { row = r; col = c }

let multiPos rows cols =
    List.allPairs rows cols
    |> List.map (fun (r, c) -> pos r c)

let allPositions = multiPos allDigits allDigits

let boxrc r c =
    match (segment r, segment c) with
    | SegOne, SegOne -> One
    | SegOne, SegTwo -> Two
    | SegOne, SegThree -> Three
    | SegTwo, SegOne -> Four
    | SegTwo, SegTwo -> Five
    | SegTwo, SegThree -> Six
    | SegThree, SegOne -> Seven
    | SegThree, SegTwo -> Eight
    | SegThree, SegThree -> Nine

let row r = multiPos [ r ] allDigits

let allRows = allDigits |> List.map row

let col c = multiPos allDigits [ c ]

let allCols = allDigits |> List.map col

let box d =
    let boxPositions s1 s2 = multiPos (segDigits s1) (segDigits s2)

    match d with
    | One -> boxPositions SegOne SegOne
    | Two -> boxPositions SegOne SegTwo
    | Three -> boxPositions SegOne SegThree
    | Four -> boxPositions SegTwo SegOne
    | Five -> boxPositions SegTwo SegTwo
    | Six -> boxPositions SegTwo SegThree
    | Seven -> boxPositions SegThree SegOne
    | Eight -> boxPositions SegThree SegTwo
    | Nine -> boxPositions SegThree SegThree

let boxp p = boxrc p.row p.col

let allBoxes = allDigits |> List.map box

let allGroups =
    List.concat [ allRows
                  allCols
                  allBoxes ]

let rowNeighbors p =
    List.filter (fun pp -> pp.col <> p.col) (row p.row)

let colNeighbors p =
    List.filter (fun pp -> pp.row <> p.row) (col p.col)

let boxNeighbors (p: Pos) : Pos list =
    let b = boxp p
    List.filter (fun (pp: Pos) -> pp <> p) (box b)

let allNeighbors p =
    let r = rowNeighbors p
    let c = colNeighbors p
    let b = boxNeighbors p
    List.concat [ r; c; b ] |> List.distinct

type CellValue =
    | Answer of Digit
    | Pencils of Set<Digit>

type Cell = { position: Pos; value: CellValue }

type PuzzleSolution = Map<Pos, Digit>
type Puzzle = Map<Pos, Cell>

let solvedCell p d = { position = p; value = Answer d }

let unsolvedCell p ds = { position = p; value = Pencils ds }

let starterCell p = unsolvedCell p allDigitsSet

let cellPencils c =
    match c.value with
    | Pencils ds -> ds
    | Answer _ -> Set.empty

let cellDigits c =
    match c.value with
    | Pencils _ -> Set.empty
    | Answer d -> Set.singleton d

let emptyPuzzle : Puzzle =
    allPositions
    |> List.map (fun p -> p, starterCell p)
    |> Map.ofList

let isCompleteSolution (s: PuzzleSolution) = s.Count = allPositions.Length

type CellFinder = Pos -> Cell

type RuleResult =
    | Solved of Digit
    | RemovePencils of Set<Digit>

type Rule = CellFinder -> Pos * RuleResult list

let applyRuleResults results puzzle =
    let solve p d pz = pz |> Map.add p (solvedCell p d)

    let remove p ds pz =
        let currentPencils = cellPencils (Map.find p pz)
        let changedPencils = currentPencils - ds
        let changedCell = unsolvedCell p changedPencils
        Map.add p changedCell pz

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemovePencils ds -> remove p ds pz

    let applied = results |> List.fold applyResult puzzle

    applied

let singlePencilRule lookup =
    allPositions
    |> List.map lookup
    |> List.map (fun c -> (c, cellDigits c))
    |> List.filter (fun (_, ds) -> ds.Count = 1)
    |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

let updatePencilsRule lookup =
    let solveGroup group =
        let cellsInGroup = group |> List.map lookup

        let digitsInGroup =
            cellsInGroup
            |> List.map cellDigits
            |> List.fold Set.union Set.empty

        let pencilsToRemoveFromCell c =
            cellPencils c |> Set.intersect digitsInGroup

        cellsInGroup
        |> List.map (fun c -> c, pencilsToRemoveFromCell c)
        |> List.filter (fun (_, ds) -> ds.Count > 0)
        |> List.map (fun (c, ds) -> c.position, RemovePencils ds)

    allGroups |> List.collect solveGroup

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
