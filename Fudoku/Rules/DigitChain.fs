module Fudoku.DigitChain

open Utils
open Domain
open Puzzle

let jumpsFrom (allPositions: FastSet<Position>) (source: Position) : (Position * Position) seq =
    let jumpsInGroup group =
        let active =
            group
            |> List.filter (FastSet.toFilter allPositions)

        if List.length active = 1 then [ (source, List.head active) ] else []

    [ (rowNeighbors source); (colNeighbors source); (boxNeighbors source) ]
    |> List.collect jumpsInGroup
    |> List.toSeq

let allPossibleJumps (allPositions: FastSet<Position>) =
    allPositions
    |> FastSet.toSeq
    |> Seq.collect (jumpsFrom allPositions)
    |> SetMap.ofPairs

let solveForPosition (positionSet: FastSet<Position>) (validJumps: SetMap<Position, Position>) (from: Position) : Position option =
    let neighborsOf pos =
        allNeighbors pos
        |> Seq.ofList
        |> Seq.filter (FastSet.toFilter positionSet)

    let rec pathsFor (current: Position) (jumps: SetMap<Position, Position>) (first: bool) : Position seq seq =
        let jumper neighbor =
            let nextJumps =
                jumps
                |> SetMap.remove current neighbor
                |> SetMap.remove neighbor current

            pathsFor neighbor nextJumps false

        let collector neighbor =
            match neighbor with
            | n when n = from -> if first then Seq.empty else Seq.singleton Seq.empty
            | n when not (SetMap.contains current n jumps) -> Seq.empty
            | n -> jumper n

        neighborsOf current
        |> Seq.collect collector
        |> Seq.map (fun path -> Seq.append path [ current ])

    let solve start =
        let startJumps = validJumps |> SetMap.remove start from
        pathsFor start startJumps true

    let isEvenPath (path: Position seq) : bool =
        let count = Seq.length path
        count > 2 && count % 2 = 0

    neighborsOf from
    |> Seq.collect solve
    |> Seq.tryFind isEvenPath
    |> Option.map (fun _ -> from)

let solveForDigit digit positionSet =
    let validJumps = allPossibleJumps positionSet

    positionSet
    |> FastSet.toSeq
    |> Seq.map (solveForPosition positionSet validJumps)
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
