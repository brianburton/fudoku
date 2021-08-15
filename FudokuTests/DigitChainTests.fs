module Fudoku.Tests.DigitChain

open NUnit.Framework
open Fudoku.Domain
open Fudoku.Puzzle
open Fudoku.PuzzleIO
open Fudoku.Tests.Common

[<Test>]
let ``simple chain of four`` () =
    let chain = [ (3, 3); (7, 3); (7, 7); (8, 9); (3, 9) ]

    let start =
        emptyPuzzle
        |> bulkRemovePencils AllPositions [ 4 ]
        |> bulkAddPencils (posList chain) [ 4; 5 ]

    let result = Fudoku.EvenPath.rule (cellFinder start)
    Assert.AreEqual(result.changes, [ ((pos (3, 3)), RemovePencils(digSet [ 4 ])) ])

[<Test>]
let ``works with extra cells in group`` () =
    let chain = [ (3, 1); (8, 3); (3, 3); (7, 3); (7, 7); (8, 9); (3, 9) ]

    let start =
        emptyPuzzle
        |> bulkRemovePencils AllPositions [ 1 ]
        |> bulkAddPencils (posList chain) [ 1; 5 ]

    let result = Fudoku.EvenPath.rule (cellFinder start)
    Assert.AreEqual(result.changes, [ ((pos (3, 3)), RemovePencils(digSet [ 4 ])) ])
