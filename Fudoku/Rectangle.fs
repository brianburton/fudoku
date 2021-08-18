module Fudoku.Rectangle

open Domain
open Puzzle
open Utils

type Rectangle<'T> = { topLeft: 'T; topRight: 'T; bottomLeft: 'T; bottomRight: 'T }

let rotateRectangle r =
    match r with
    | { topLeft = tl; topRight = tr; bottomLeft = bl; bottomRight = br } -> { topLeft = bl; topRight = tl; bottomLeft = br; bottomRight = tr }

let twoBoxRectangles =
    let cornersToRect corners =
        match corners with
        | tl :: tr :: bl :: [ br ] -> { topLeft = tl; topRight = tr; bottomLeft = bl; bottomRight = br }
        | _ -> invalidArg "corners" $"should have 4 values but had {corners.Length}"

    let isTwoBoxer rect =
        Set.empty
        |> Set.add (boxContainingPos rect.topLeft)
        |> Set.add (boxContainingPos rect.topRight)
        |> Set.add (boxContainingPos rect.bottomLeft)
        |> Set.add (boxContainingPos rect.bottomRight)
        |> Set.count = 2

    let pairs = combinations 2 AllDigits

    List.allPairs pairs pairs
    |> List.map (fun (rows, cols) -> positions rows cols)
    |> List.map cornersToRect
    |> List.filter isTwoBoxer

type CellSummary = { cellPos: Position; cellPencils: Set<Digit> }

let summarizeCell lookup pos = { cellPos = pos; cellPencils = cellPencils (lookup pos) }

let summarizeRectangle (lookup: CellFinder) (posRect: Rectangle<Position>) =
    match posRect with
    | { topLeft = tl; topRight = tr; bottomLeft = bl; bottomRight = br } ->
        { topLeft = summarizeCell lookup bl
          topRight = summarizeCell lookup tl
          bottomLeft = summarizeCell lookup br
          bottomRight = summarizeCell lookup tr }

let solveUniqueRectangle lookup rectangle =
    let singleCellWithExtraPencils pencils c d =
        if pencils = c.cellPencils
           && pencils = (Set.intersect d.cellPencils pencils)
           && d.cellPencils.Count <> 2 then
            Some [ (d.cellPos, RemovePencils pencils) ]
        else
            None

    let twoCellsSharingOneExtraDigit pencils c d =
        let extraDigits = Set.difference c.cellPencils pencils

        if (Set.intersect pencils c.cellPencils) <> pencils
           || c.cellPencils <> d.cellPencils
           || extraDigits.Count <> 1 then
            None
        else
            commonNeighbors c.cellPos d.cellPos
            |> List.map lookup
            |> List.filter (fun x -> Set.isSuperset (cellPencils x) extraDigits)
            |> List.map (fun x -> (x.position, RemovePencils extraDigits))
            |> Some

    let twoCellsActingAsPair pencils c d =
        let extraDigits =
            Set.union c.cellPencils d.cellPencils
            |> (fun u -> Set.difference u pencils)

        if (Set.intersect pencils c.cellPencils) <> pencils
           || (Set.intersect pencils d.cellPencils) <> pencils
           || extraDigits.Count <> 2 then
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
                    |> List.filter (fun x -> (setsOverlap (cellPencils x) extraDigits))
                    |> List.map (fun x -> (x.position, RemovePencils extraDigits)))
            |> Some

    let solve a c d =
        singleCellWithExtraPencils a.cellPencils c d
        |> Option.orElseWith (fun () -> twoCellsSharingOneExtraDigit a.cellPencils c d)
        |> Option.orElseWith (fun () -> twoCellsActingAsPair a.cellPencils c d)


    match rectangle with
    | { topLeft = a; topRight = b; bottomLeft = c; bottomRight = d } ->
        if a.cellPencils.Count <> 2
           || b.cellPencils <> a.cellPencils then
            None
        else
            solve a c d

let solveUniqueRectangleRotations rectangle (lookup: CellFinder) =
    let rec rotateAndSolve count rectangle =
        if count = 0 then
            None
        else
            let solution = solveUniqueRectangle lookup rectangle

            match solution with
            | None -> rotateAndSolve (count - 1) (rotateRectangle rectangle)
            | Some _ -> solution

    rotateAndSolve 4 rectangle

let singleUniqueRectangleRule rectangle (lookup: CellFinder) =
    let summarized = summarizeRectangle lookup rectangle
    let changes = solveUniqueRectangleRotations summarized lookup
    { rule = "unique-rectangle"; changes = Option.defaultValue List.empty changes }

let uniqueRectangleRules =
    twoBoxRectangles
    |> List.map singleUniqueRectangleRule
