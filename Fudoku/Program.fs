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

type Dig =
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

let digApply f =
    [ for d in allDigits do
          yield (f d) ]

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

type Pos = { row: Dig; col: Dig }

let pos r c = { row = r; col = c }

let positions rows cols =
    List.allPairs rows cols
    |> List.map (fun (r, c) -> pos r c)

let allPositions = positions allDigits allDigits

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

let row r = positions [ r ] allDigits
let rowM = memoize row

let allRows = allDigits |> List.map row

let col c = positions allDigits [ c ]
let colM = memoize col

let allCols = allDigits |> List.map col

let box d =
    let boxPositions s1 s2 = positions (segDigits s1) (segDigits s2)

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

let boxM = memoize box

let boxp p = boxrc p.row p.col

let allBoxes = allDigits |> List.map box

let allGroups =
    List.concat [ allRows
                  allCols
                  allBoxes ]

let rowNeighbors p =
    List.filter (fun pp -> pp.col <> p.col) (row p.row)

let rowNeighborsM = memoize rowNeighbors

let colNeighbors p =
    List.filter (fun pp -> pp.row <> p.row) (col p.col)

let colNeighborsM = memoize colNeighbors

let boxNeighbors (p: Pos) : Pos list =
    let b = boxp p
    List.filter (fun (pp: Pos) -> pp <> p) (box b)

let boxNeighborsM = memoize boxNeighbors

let allNeighbors p =
    let r = rowNeighbors p
    let c = colNeighbors p
    let b = boxNeighbors p
    List.concat [ r; c; b ] |> List.distinct

let allNeighborsM = memoize allNeighbors

type Cell =
    { pos: Pos
      pencils: Set<Dig>
      dig: Dig option }

type Solution = Map<Pos, Dig option>
type Puzzle = Map<Pos, Cell>

type PuzzleSolution = { puzzle: Puzzle; solution: Solution }

let solvedCell p d =
    { pos = p
      pencils = Set.empty
      dig = (Some d) }

let unsolvedCell p ds = { pos = p; pencils = ds; dig = None }

let starterCell p = unsolvedCell p allDigitsSet

let emptyPuzzle : Puzzle =
    allPositions
    |> List.map (fun p -> p, starterCell p)
    |> Map.ofList

let emptySolution : Solution =
    allPositions
    |> List.map (fun p -> p, None)
    |> Map.ofList

let emptyPuzzleSolution =
    { puzzle = emptyPuzzle
      solution = emptySolution }

let isCompleteSolution (s: Solution) =
    let sizeOk = s.Count = allPositions.Length

    let anyMissing =
        Map.exists (fun _ (cell: Dig option) -> cell.IsNone)

    sizeOk && not (anyMissing s)

type CellFinder = Pos -> Cell

type RuleResult =
    | Solved of Dig
    | RemoveDigits of Set<Dig>

type Rule = CellFinder -> Pos * RuleResult list

let applyRuleResults results puzzle =
    let solve p d pz = Map.add p (solvedCell p d) pz

    let remove p ds pz =
        let currentPencils = (Map.find p pz).pencils
        let changedPencils = currentPencils - ds
        let changedCell = unsolvedCell p changedPencils
        Map.add p changedCell pz

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemoveDigits ds -> remove p ds pz

    let applied = results |> List.fold applyResult puzzle

    applied

let cellDigits ps lookup =
    let cellDigits c =
        match c.dig with
        | Some d -> [ d ]
        | None -> []

    List.map lookup ps
    |> List.collect cellDigits
    |> Set.ofList

let singlePencilRule lookup =
    List.map lookup allPositions
    |> List.filter (fun c -> c.pencils.Count = 1)
    |> List.map (fun c -> c.pos, Solved c.pencils.MinimumElement)

let updatePencilsRule lookup =
    let solveGroup group =
        let groupDigits = cellDigits group lookup

        group
        |> List.map lookup
        |> List.map (fun c -> c, Set.intersect c.pencils groupDigits)
        |> List.filter (fun (_, ds) -> ds.Count > 0)
        |> List.map (fun (c, ds) -> c.pos, RemoveDigits ds)

    allGroups |> List.collect solveGroup

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
