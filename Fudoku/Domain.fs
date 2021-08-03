module Fudoku.Domain

open Fudoku.Utils

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

type Segment =
    | SegOne
    | SegTwo
    | SegThree

type Position = { row: Digit; col: Digit }

type CellValue =
    | Answer of Digit
    | Pencils of Set<Digit>

type Cell =
    { position: Position
      value: CellValue }

type CellFinder = Position -> Cell

type DigitCombination =
    { inside: Digit list
      outside: Digit list }

type PositionCombination =
    { inside: Position list
      outside: Position list }

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

let private createDigitCombos len =
    let comboOf digits : DigitCombination =
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

let cellContainsPencils c ds =
    let pencils = cellPencils c
    let common = Set.intersect ds pencils
    (Set.count common) > 0

let cellGroupPencils group =
    group
    |> List.map cellPencils
    |> List.fold Set.union Set.empty
