module Fudoku.Rectangle

open Domain
open Puzzle
open Utils

type Rectangle<'T> = { topLeft: 'T; topRight: 'T; bottomLeft: 'T; bottomRight: 'T }

let rotateRectangle r =
    match r with
    | { topLeft = tl; topRight = tr; bottomLeft = bl; bottomRight = br } -> { topLeft = bl; topRight = tl; bottomLeft = br; bottomRight = tr }

let isTwoBoxer rect =
    NoDigits
    |> FastSet.add (boxContainingPos rect.topLeft)
    |> FastSet.add (boxContainingPos rect.topRight)
    |> FastSet.add (boxContainingPos rect.bottomLeft)
    |> FastSet.add (boxContainingPos rect.bottomRight)
    |> FastSet.length = 2

let cornersToRect (corners: 'a list) =
    { topLeft = corners.[0]
      topRight = corners.[1]
      bottomLeft = corners.[2]
      bottomRight = corners.[3] }

let isValidRect r =
    r.topLeft.row = r.topRight.row
    && r.bottomLeft.row = r.bottomRight.row
    && r.topLeft.col = r.bottomLeft.col
    && r.topRight.col = r.bottomRight.col

type CellSummary = { cellPos: Position; cellPencils: FastSet<Digit> }

let summarizeCell lookup pos = { cellPos = pos; cellPencils = cellPencils (lookup pos) }

let summarizeRectangle (lookup: CellFinder) (posRect: Rectangle<Position>) =
    match posRect with
    | { topLeft = tl; topRight = tr; bottomLeft = bl; bottomRight = br } ->
        { topLeft = summarizeCell lookup bl
          topRight = summarizeCell lookup tl
          bottomLeft = summarizeCell lookup br
          bottomRight = summarizeCell lookup tr }

let intersectTwoSetsInMap setMap a b =
    let setA = SetMap.get a setMap
    let setB = SetMap.get b setMap
    FastSet.intersect setA setB

let activeRectangles digitMap =
    let digits = SetMap.keys digitMap |> Seq.toList
    let pairs = combinations 2 digits

    pairs
    |> List.map (fun ds -> intersectTwoSetsInMap digitMap ds.[0] ds.[1])
    |> List.filter (fun s -> FastSet.length s >= 4)
    |> List.map FastSet.toList
    |> List.map List.sort
    |> List.collect (combinations 4)
    |> Seq.ofList
    |> Seq.map cornersToRect
    |> Seq.filter isValidRect
    |> Seq.filter isTwoBoxer

let solveUniqueRectangle lookup rectangle =
    let singleCellWithExtraPencils pencils c d =
        if (FastSet.equals pencils c.cellPencils)
           && (FastSet.equals pencils (FastSet.intersect d.cellPencils pencils))
           && (FastSet.length d.cellPencils) <> 2 then
            Some [ (d.cellPos, RemovePencils pencils) ]
        else
            None

    let twoCellsSharingOneExtraDigit pencils c d =
        let extraDigits = FastSet.difference c.cellPencils pencils

        if (FastSet.notEquals (FastSet.intersect pencils c.cellPencils) pencils)
           || (FastSet.notEquals c.cellPencils d.cellPencils)
           || (FastSet.length extraDigits) <> 1 then
            None
        else
            commonNeighbors c.cellPos d.cellPos
            |> List.map lookup
            |> List.filter (fun x -> FastSet.isSuperset (cellPencils x) extraDigits)
            |> List.map (fun x -> (x.position, RemovePencils extraDigits))
            |> listToOption

    let twoCellsActingAsPair pencils c d =
        let extraDigits =
            FastSet.union c.cellPencils d.cellPencils
            |> (fun u -> FastSet.difference u pencils)

        if (FastSet.notEquals (FastSet.intersect pencils c.cellPencils) pencils)
           || (FastSet.notEquals (FastSet.intersect pencils d.cellPencils) pencils)
           || (FastSet.length extraDigits) <> 2 then
            None
        else
            let commonPositions = commonNeighbors c.cellPos d.cellPos

            commonPositions
            |> List.map lookup
            |> List.filter (fun x -> (cellPencils x) = extraDigits)
            |> List.collect
                (fun cell ->
                    intersectLists commonPositions (allNeighbors cell.position)
                    |> List.map lookup
                    |> List.filter (fun x -> (FastSet.overlaps (cellPencils x) extraDigits))
                    |> List.map (fun x -> (x.position, RemovePencils extraDigits)))
            |> listToOption

    let solve a c d =
        singleCellWithExtraPencils a.cellPencils c d
        |> Option.orElseWith (fun () -> twoCellsSharingOneExtraDigit a.cellPencils c d)
        |> Option.orElseWith (fun () -> twoCellsActingAsPair a.cellPencils c d)

    match rectangle with
    | { topLeft = a; topRight = b; bottomLeft = c; bottomRight = d } ->
        if (FastSet.length a.cellPencils) <> 2
           || (FastSet.notEquals b.cellPencils a.cellPencils) then
            None
        else
            solve a c d

let rotationsOf rect =
    seq {
        let mutable r = rect

        for _ in 1 .. 4 do
            yield r
            r <- rotateRectangle r
    }

let uniqueRectangleRule (lookup: CellFinder) : RuleResult =
    let activeDigitMap = createDigitMap AllPositions lookup
    let candidateRectangles = activeRectangles activeDigitMap

    let result =
        candidateRectangles
        |> Seq.map (summarizeRectangle lookup)
        |> Seq.collect rotationsOf
        |> Seq.map (solveUniqueRectangle lookup)
        |> Seq.collect Option.toList
        |> Seq.truncate 1
        |> Seq.toList

    let changes =
        match result with
        | list :: _ -> list
        | _ -> []

    { rule = "unique-rectangle"; changes = changes }
