module Fudoku.DigitChain

open Utils
open Domain
open Puzzle

let jumpsFrom (source: Position) (positions: FastSet<Position>) : FastSet<Position * Position> =
    let jumpsInGroup group =
        let active = FastSet.ofSeq group |> FastSet.intersect positions

        if FastSet.length active = 1 then [ (source, FastSet.head active) ] else []

    [ (rowNeighbors source); (colNeighbors source); (boxNeighbors source) ]
    |> List.collect jumpsInGroup
    |> FastSet.ofSeq

let allPossibleJumps (positionList: List<Position>) (positionSet: FastSet<Position>) =
    positionList
    |> List.map (swapArgs jumpsFrom positionSet)
    |> List.fold FastSet.union (FastSet.empty ())

let solveForPosition (from: Position) (positionSet: FastSet<Position>) (validJumps: FastSet<Position * Position>) : Position option =
    let neighborsOf pos =
        allNeighbors pos
        |> Seq.ofList
        |> Seq.filter (FastSet.toFilter positionSet)

    let rec pathsFor (current: Position) (jumps: FastSet<Position * Position>) (first: bool) : Position seq seq =
        let jumper neighbor =
            let nextJumps =
                jumps
                |> FastSet.remove (current, neighbor)
                |> FastSet.remove (neighbor, current)

            pathsFor neighbor nextJumps false

        let collector neighbor =
            match neighbor with
            | n when n = from -> if first then Seq.empty else Seq.singleton Seq.empty
            | n when not (FastSet.contains (current, n) jumps) -> Seq.empty
            | n -> jumper n

        neighborsOf current
        |> Seq.collect collector
        |> Seq.map (fun path -> Seq.append path [ current ])

    let solve start =
        let startJumps = validJumps |> FastSet.remove (start, from)
        pathsFor start startJumps true

    let isEvenPath (path: Position seq) : bool =
        let count = Seq.length path
        count > 2 && count % 2 = 0

    neighborsOf from
    |> Seq.collect solve
    |> Seq.tryFind isEvenPath
    |> Option.map (fun _ -> from)

let solveForDigit digit positionSet =
    let positionList = positionSet |> FastSet.toList
    let jumpSet = allPossibleJumps positionList positionSet

    positionList
    |> Seq.ofList
    |> Seq.map (fun p -> solveForPosition p positionSet jumpSet)
    |> Seq.tryFind Option.isSome
    |> Option.bind id
    |> Option.map (fun p -> p, RemovePencils(FastSet.singleton digit))
    |> Option.toList

let rule lookup =
    let digitMap = createDigitMap AllPositions lookup

    let changes =
        SetMap.keys digitMap
        |> Seq.map (fun digit -> solveForDigit digit (SetMap.get digit digitMap))
        |> firstNonEmptyList

    { rule = "digit-chain"; changes = changes }
