module Fudoku.Tests

open NUnit.Framework
open Domain
open Puzzle

let unsolvedCell r c ds =
    unsolvedCell (position r c) (Set.ofList ds)

[<SetUp>]
let Setup () = ()

[<Test>]
let singlePencilTests () =
    let p11 = position One One
    let p19 = position One Nine

    let before =
        addToPuzzle
            emptyPuzzle
            [ (unsolvedCell One One [ Two ])
              (unsolvedCell One Nine [ Eight ]) ]

    let expected = [ p11, Solved Two; p19, Solved Eight ]

    let actual = SingleDigit.rule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)
    Assert.Pass()
