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
    override this.ToString() =
        match this with
        | One -> "1"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"

type Segment =
    | SegOne
    | SegTwo
    | SegThree
    override this.ToString() =
        match this with
        | SegOne -> "S1"
        | SegTwo -> "S2"
        | SegThree -> "S3"

type Position =
    { row: Digit
      col: Digit }
    override this.ToString() = $"({this.row},{this.col})"

type CellValue =
    | Answer of Digit
    | Pencils of FastSet<Digit>

type Cell = { position: Position; value: CellValue }

type CellFinder = Position -> Cell

type Combination<'T> = { inside: 'T list; outside: 'T list }

type CellSummary = { cellPos: Position; cellPencils: FastSet<Digit> }

let AllDigits = [ One; Two; Three; Four; Five; Six; Seven; Eight; Nine ]

let AllDigitsSet = AllDigits |> FastSet.ofSeq

let NoDigits: FastSet<Digit> = FastSet.empty ()

let groupCombo group digits =
    let others = group |> List.except digits
    { inside = digits; outside = others }

let digitCombo digits : Combination<Digit> = groupCombo AllDigits digits

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

let segmentDigits s =
    match s with
    | SegOne -> [ One; Two; Three ]
    | SegTwo -> [ Four; Five; Six ]
    | SegThree -> [ Seven; Eight; Nine ]

let SingleSegmentDigitTriples =
    [ digitCombo (segmentDigits SegOne)
      digitCombo (segmentDigits SegTwo)
      digitCombo (segmentDigits SegThree) ]

let position r c = { row = r; col = c }

let pairToPosition = unpair position

let positions rows cols =
    List.allPairs rows cols
    |> List.map pairToPosition

let AllPositions = positions AllDigits AllDigits
let NoPositions: FastSet<Position> = FastSet.empty ()

let combinationMapper (mapping: Digit -> Position) (combo: Combination<Digit>) : Combination<Position> =
    let posInside = combo.inside |> List.map mapping
    let posOutside = combo.outside |> List.map mapping

    { inside = posInside; outside = posOutside }

let boxContaining r c =
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

let boxContainingPos p = boxContaining p.row p.col

let row r = positions [ r ] AllDigits

let AllRows = AllDigits |> List.map row

let col c = positions AllDigits [ c ]

let AllCols = AllDigits |> List.map col

let box d =
    let boxPositions s1 s2 = positions (segmentDigits s1) (segmentDigits s2)

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

let AllBoxes = AllDigits |> List.map box

let AllGroups =
    List.concat [ AllRows
                  AllCols
                  AllBoxes ]

let rowNeighbors =
    let impl (p: Position) = List.filter (fun pp -> pp.col <> p.col) (row p.row)
    memoize impl

let colNeighbors =
    let impl (p: Position) = List.filter (fun pp -> pp.row <> p.row) (col p.col)
    memoize impl

let boxNeighbors =
    let impl (p: Position) =
        boxContainingPos p
        |> box
        |> List.filter (fun (pp: Position) -> pp <> p)

    memoize impl

let allNeighbors =
    let impl p =
        let r = rowNeighbors p
        let c = colNeighbors p
        let b = boxNeighbors p
        List.concat [ r; c; b ] |> List.distinct

    memoize impl

let commonNeighborsPaired =
    let impl (a, b) = intersectLists (allNeighbors a) (allNeighbors b)
    memoize impl

let commonNeighbors p1 p2 = commonNeighborsPaired (p1, p2)

let solvedCell p d = { position = p; value = Answer d }

let unsolvedCell p ds = { position = p; value = Pencils ds }

let starterCell p = unsolvedCell p AllDigitsSet

let cellPencils c =
    match c.value with
    | Pencils ds -> ds
    | Answer _ -> NoDigits

let cellDigit c =
    match c.value with
    | Pencils _ -> NoDigits
    | Answer d -> FastSet.singleton d

let cellContainsPencil d c =
    match c.value with
    | Pencils cds -> FastSet.contains d cds
    | Answer _ -> false

let cellContainsPencils ds c =
    match c.value with
    | Pencils cds -> FastSet.overlaps cds ds
    | Answer _ -> false

let groupPencils group =
    group
    |> List.map cellPencils
    |> List.fold FastSet.union NoDigits

let summarizeCell lookup pos = { cellPos = pos; cellPencils = cellPencils (lookup pos) }

let lookupCellCombination (lookup: CellFinder) (combo: Combination<Position>) =
    let insideCells = combo.inside |> List.map lookup
    let outsideCells = combo.outside |> List.map lookup
    { inside = insideCells; outside = outsideCells }

let cellPencilList cell =
    cellPencils cell
    |> FastSet.toList
    |> List.map (fun d -> (d, cell.position))

let createDigitMap (group: Position list) (lookup: CellFinder) : SetMap<Digit, Position> =
    group
    |> List.map lookup
    |> List.collect cellPencilList
    |> SetMap.ofPairs

let validTupleLength max actual = (actual >= 2) && (actual <= max)

let validTupleList len ps = validTupleLength len (List.length ps)

let validTupleSet len ps = validTupleLength len (FastSet.length ps)
