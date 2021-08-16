module Fudoku.DigitChain

open Domain
open Puzzle
open Utils

let cellPencilList cell =
    cellPencils cell
    |> Set.toList
    |> List.map (fun d -> (d, cell.position))

let createDigitMap (lookup: CellFinder) : Map<Digit, Set<Position>> =
    AllPositions
    |> List.map lookup
    |> List.collect cellPencilList
    |> SetMap.ofPairs

let jumpsFrom (source: Position) (positions: Set<Position>) : Set<Position * Position> =
    let jumpsInGroup group =
        let active = Set.ofList group |> Set.intersect positions

        if Set.count active = 1 then [ (source, Set.minElement active) ] else []

    [ (rowNeighbors source); (colNeighbors source); (boxNeighbors source) ]
    |> List.collect jumpsInGroup
    |> Set.ofList

let allPossibleJumps (positionList: List<Position>) (positionSet: Set<Position>) =
    positionList
    |> List.map (fun p -> jumpsFrom p positionSet)
    |> List.fold (fun ps p -> Set.union p ps) Set.empty

let solveForPosition (from: Position) (positionSet: Set<Position>) (validJumps: Set<Position * Position>) : Position option =
    let neighborsOf pos =
        allNeighbors pos
        |> Seq.ofList
        |> Seq.filter (setContainsElement positionSet)

    let rec pathsFor (current: Position) (jumps: Set<Position * Position>) (first: bool) : Position seq seq =
        let jumper neighbor =
            let nextJumps =
                jumps
                |> Set.remove (current, neighbor)
                |> Set.remove (neighbor, current)

            pathsFor neighbor nextJumps false

        let collector neighbor =
            match neighbor with
            | n when n = from -> if first then Seq.empty else Seq.singleton Seq.empty
            | n when not (Set.contains (current, n) jumps) -> Seq.empty
            | n -> jumper n

        neighborsOf current
        |> Seq.collect collector
        |> Seq.map (fun path -> Seq.append path [ current ])

    let solve start =
        let startJumps = validJumps |> Set.remove (start, from)
        pathsFor start startJumps true

    let isEvenPath (path: Position seq) : bool =
        let count = Seq.length path
//            if count > 0 then printf $"{from} -> %A{path}\n"
        count > 2 && count % 2 = 0

    neighborsOf from
    |> Seq.collect solve
    |> Seq.tryFind isEvenPath
    |> Option.map (fun _ -> from)

let solveForDigit digit positionSet =
    let positionList = positionSet |> Set.toList
    let jumpSet = allPossibleJumps positionList positionSet

    positionList
    |> Seq.ofList
    |> Seq.map (fun p -> solveForPosition p positionSet jumpSet)
    |> Seq.tryFind Option.isSome
    |> Option.bind id
    |> Option.map (fun p -> p, RemovePencils(Set.singleton digit))
    |> Option.toList

let rule lookup =
    let digitMap = createDigitMap lookup

    let changes =
        SetMap.keys digitMap
        |> Seq.map (fun digit -> solveForDigit digit (SetMap.get digit digitMap))
        |> Seq.tryFind (fun changes -> not (List.isEmpty changes))
        |> Option.defaultValue []

    { rule = "digit-chain"; changes = changes }
