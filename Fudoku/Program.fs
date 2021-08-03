open System.Text.RegularExpressions

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

let rec combinations len list =
    let length1 () = list |> List.map (fun x -> [ x ])

    let single () = [ list ]

    let merge item lst = lst |> List.map (fun x -> item :: x)

    let normal head tail =
        let tails = combinations len tail
        let suffixes = combinations (len - 1) tail
        let merges = merge head suffixes
        List.append merges tails

    if len = 1 then length1 ()
    elif len = list.Length then single ()
    else normal list.Head list.Tail

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

type DigitCombination =
    { inside: Digit list
      outside: Digit list }

let createDigitCombos len =
    let comboOf digits =
        let others = allDigits |> List.except digits
        { inside = digits; outside = others }

    combinations len allDigits |> List.map comboOf

let DigitSingles = createDigitCombos 1
let DigitPairs = createDigitCombos 2
let DigitTriples = createDigitCombos 3
let DigitQuads = createDigitCombos 4

let AllDigitCombinations =
    DigitSingles
    |> List.append DigitPairs
    |> List.append DigitTriples
    |> List.append DigitQuads

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

type Position = { row: Digit; col: Digit }

type PositionCombination =
    { inside: Position list
      outside: Position list }

let pos r c = { row = r; col = c }

let multiPos rows cols =
    List.allPairs rows cols
    |> List.map (fun (r, c) -> pos r c)

let allPositions = multiPos allDigits allDigits

let combinationMapper (mapping: Digit -> Position) (combo: DigitCombination) : PositionCombination =
    let posInside = combo.inside |> List.map mapping
    let posOutside = combo.outside |> List.map mapping

    { inside = posInside
      outside = posOutside }

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

let boxNeighbors (p: Position) : Position list =
    let b = boxp p
    List.filter (fun (pp: Position) -> pp <> p) (box b)

let allNeighbors p =
    let r = rowNeighbors p
    let c = colNeighbors p
    let b = boxNeighbors p
    List.concat [ r; c; b ] |> List.distinct

type CellValue =
    | Answer of Digit
    | Pencils of Set<Digit>

type Cell =
    { position: Position
      value: CellValue }

type PuzzleSolution = Map<Position, Digit>
type Puzzle = Map<Position, Cell>

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

type CellFinder = Position -> Cell

let cellFinder pz = fun p -> Map.find p pz

type RuleResultChange =
    | Solved of Digit
    | RemovePencils of Set<Digit>
    | RetainPencils of Set<Digit>

type RuleResult =
    { rule: string
      changes: (Position * RuleResultChange) list }

type Rule = CellFinder -> RuleResult

let applyRuleResults results puzzle =
    let solve p d pz = pz |> Map.add p (solvedCell p d)

    let pencils p ds pz op =
        let currentPencils = cellPencils (Map.find p pz)
        let changedPencils = ds |> op currentPencils
        let changedCell = unsolvedCell p changedPencils
        Map.add p changedCell pz

    let remove p ds pz = pencils p ds pz Set.difference

    let retain p ds pz = pencils p ds pz Set.intersect

    let applyResult pz (p, result) =
        match result with
        | Solved d -> solve p d pz
        | RemovePencils ds -> remove p ds pz
        | RetainPencils ds -> retain p ds pz

    let applied = results |> List.fold applyResult puzzle

    applied

let applyRules lookup rules =
    let applyRule prior rule =
        if List.isEmpty prior.changes then
            rule lookup
        else
            prior

    rules
    |> List.fold applyRule { rule = ""; changes = List.empty }

let cellGroupPencils group =
    group
    |> List.map cellPencils
    |> List.fold Set.union Set.empty

let singlePencilRule lookup =
    let changes =
        allPositions
        |> List.map lookup
        |> List.map (fun c -> (c, cellPencils c))
        |> List.filter (fun (_, ds) -> ds.Count = 1)
        |> List.map (fun (c, ds) -> c.position, Solved ds.MinimumElement)

    { rule = "single-pencil"
      changes = changes }

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

    let changes = allGroups |> List.collect solveGroup

    { rule = "fix-pencils"
      changes = changes }

let mapDigitsToGroup group =
    let map = List.zip allDigits group |> Map.ofList
    (fun digit -> Map.find digit map)

let hiddenPencilsRule (group: Position list) (combo: DigitCombination) (lookup: CellFinder) =
    let len = combo.inside.Length

    let mapper = mapDigitsToGroup group

    let insideCells =
        combo.inside |> List.map mapper |> List.map lookup

    let outsideCells =
        combo.outside
        |> List.map mapper
        |> List.map lookup

    let insideDigits = cellGroupPencils insideCells
    let outsideDigits = cellGroupPencils outsideCells
    let uniqueDigits = insideDigits - outsideDigits

    let changes =
        if uniqueDigits.Count <> len
           || uniqueDigits = insideDigits then
            List.empty
        else
            insideCells
            |> List.map (fun c -> c.position, RetainPencils uniqueDigits)

    { rule = "hidden-pencils"
      changes = changes }

let allHiddenPencilRules =
    List.allPairs allGroups AllDigitCombinations
    |> List.map (fun (group, combo) -> hiddenPencilsRule group combo)

let AllRules =
    allHiddenPencilRules
    |> List.append [ updatePencilsRule
                     singlePencilRule ]

let stringToPuzzle source =
    let charToDigit c =
        match c with
        | '1' -> Some One
        | '2' -> Some Two
        | '3' -> Some Three
        | '4' -> Some Four
        | '5' -> Some Five
        | '6' -> Some Six
        | '7' -> Some Seven
        | '8' -> Some Eight
        | '9' -> Some Nine
        | _ -> None

    let createCell (pos, digit) =
        match digit with
        | Some d -> pos, solvedCell pos d
        | None -> pos, starterCell pos

    let filtered =
        Regex.Replace(source, "[^0123456789.]", "")

    let digits =
        filtered |> Seq.map charToDigit |> Seq.toList

    List.zip allPositions digits
    |> List.map createCell
    |> Map.ofList

type SolutionStep = { rule: string; puzzle: Puzzle }

let solvePuzzle puzzle =
    let applyAllRulesToPuzzle pz = applyRules (cellFinder pz) AllRules

    let mutable steps =
        List.singleton { rule = "start"; puzzle = puzzle }

    let mutable puzzle2 = puzzle
    let mutable results = applyRules (cellFinder puzzle2) AllRules

    while results.changes.Length > 0 do
        puzzle2 <- applyRuleResults results.changes puzzle2

        let step =
            { rule = results.rule
              puzzle = puzzle2 }

        steps <- steps @ [ step ]
        results <- puzzle2 |> applyAllRulesToPuzzle

    (puzzle2, steps)

[<EntryPoint>]
let main _ =
    let source =
        "5..86279.  .........  ...9.3.48 ......5..  1.97.52.4 ..7......  91.5.6...  .........  .86419..7"

    let pz = stringToPuzzle source
    let solved = solvePuzzle pz

    0 // return an integer exit code
