module Fudoku.XYWing

open Utils
open Domain
open Puzzle

let rule lookup =
    let activeCells =
        AllPositions
        |> List.map (summarizeCell lookup)
        |> List.filter (fun s -> FastSet.length s.cellPencils = 2)
        |> Seq.ofList

    let activePositions =
        activeCells
        |> Seq.map (fun s -> s.cellPos)
        |> FastSet.ofSeq

    let isActivePosition pos = FastSet.contains pos activePositions
    let isActiveCell cell = FastSet.contains cell.cellPos activePositions

    let digitNeighbors p d =
        seq {
            rowNeighbors p
            colNeighbors p
            boxNeighbors p
        }
        |> Seq.map (List.map (summarizeCell lookup))
        |> Seq.map (List.filter (fun n -> FastSet.contains d n.cellPencils))
        |> Seq.filter (fun ns -> List.length ns = 1)
        |> Seq.map List.head
        |> Seq.filter isActiveCell

    let solved n removeDigit =
        FastSet.difference n.cellPencils removeDigit
        |> FastSet.head
        |> Solved

    let third a c =
        let removeDigit = FastSet.intersect a.cellPencils c.cellPencils

        let neighbors =
            commonNeighbors a.cellPos c.cellPos
            |> List.filter isActivePosition
            |> List.map (summarizeCell lookup)

        neighbors
        |> List.filter (fun n -> FastSet.overlaps removeDigit n.cellPencils)
        |> List.map (fun n -> n.cellPos, solved n removeDigit)

    let second a d1 b =
        let cds =
            FastSet.union a.cellPencils b.cellPencils
            |> FastSet.remove d1

        let d2 = FastSet.remove d1 b.cellPencils |> FastSet.head

        digitNeighbors b.cellPos d2
        |> Seq.filter (fun c -> c.cellPencils = cds)
        |> Seq.map (third a)

    let first a =
        let solveForDigit d1 =
            digitNeighbors a.cellPos d1
            |> Seq.collect (second a d1)

        a.cellPencils
        |> FastSet.toSeq
        |> Seq.collect solveForDigit

    let changes =
        activeCells
        |> Seq.collect first
        |> firstNonEmptyList

    { rule = "xy-wing"; changes = changes }
