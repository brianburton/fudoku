module FudokuTests

open NUnit.Framework
open Fudoku.Domain
open Fudoku.Puzzle

[<SetUp>]
let Setup () = ()

[<Test>]
let singlePencilTests () =
    let p11 = position One One
    let p19 = position One Nine

    let before =
        addToPuzzle
            emptyPuzzle
            [ (unsolvedCell p11 (Set.singleton Two))
              (unsolvedCell p19 (Set.singleton Eight)) ]

    let expected = [ p11, Solved Two; p19, Solved Eight ]

    let actual =
        Fudoku.SingleDigit.rule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)
    Assert.Pass()
