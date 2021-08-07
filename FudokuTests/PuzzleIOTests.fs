module Fudoku.Tests.PuzzleIO

open NUnit.Framework
open Fudoku.Domain
open Fudoku.Puzzle
open Fudoku.PuzzleIO
open Fudoku.Tests.Common

[<Test>]
let groupDigits () =
    let puzzle =
        emptyPuzzle
        |> addCell (solvedCellRC One One One)
        |> addCell (solvedCellRC One Two Two)
        |> addCell (solvedCellRC One Three Three)
        |> addCell (solvedCellRC One Four Four)
        |> addCell (solvedCellRC One Five Five)
        |> addCell (solvedCellRC One Six Six)
        |> addCell (solvedCellRC One Seven Seven)
        |> addCell (solvedCellRC One Eight Eight)
        |> addCell (solvedCellRC One Nine Nine)

    let lookup = cellFinder puzzle
    let group = List.map lookup (row One)
    Assert.AreEqual("| 1 2 3 | 4 5 6 | 7 8 9 |", solvedCellsToString group)
    Assert.AreEqual("|                               |                               |                               |", unsolvedCellsToString group)

    let emptyGroup = List.map lookup (row Two)
    Assert.AreEqual("|       |       |       |", solvedCellsToString emptyGroup)
    Assert.AreEqual("| 123456789 123456789 123456789 | 123456789 123456789 123456789 | 123456789 123456789 123456789 |", unsolvedCellsToString emptyGroup)

    let mixedGroup = List.map lookup (col Two)
    Assert.AreEqual("| 2     |       |       |", solvedCellsToString mixedGroup)
    Assert.AreEqual("|           123456789 123456789 | 123456789 123456789 123456789 | 123456789 123456789 123456789 |", unsolvedCellsToString mixedGroup)

    printfn $"%s{puzzleToString puzzle}"
