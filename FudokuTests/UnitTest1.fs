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

[<Test>]
let hiddenPairTest () =
    let group = row (One)

    let combo : DigitCombination =
        { inside = [ One; Two ]
          outside =
              [ Three
                Four
                Five
                Six
                Seven
                Eight
                Nine ] }

    let before =
        addToPuzzle
            emptyPuzzle
            [ (unsolvedCell One Three [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Four [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Five [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Six [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Seven [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Eight [ One;Two;Four;Five; Six; Seven;Eight ])
              (unsolvedCell One Nine [ One;Two;Four;Five; Six; Seven;Eight ]) ]

    let retained = RetainPencils (Set.ofList [Three;Nine])
    let expected = [ (position One One),retained ; (position One Two),retained ]

    let actual = hiddenPencilsRule group combo (cellFinder before)
    Assert.AreEqual(expected, actual.changes)

    Assert.Pass()
