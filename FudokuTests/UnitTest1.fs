module Fudoku.Tests

open NUnit.Framework
open Domain
open Puzzle

let unsolvedCell r c ds =
    unsolvedCell (position r c) (Set.ofList ds)

let addCell cell map = Map.add cell.position cell map

[<SetUp>]
let Setup () = ()

[<Test>]
let singlePencilTests () =
    let p11 = position One One
    let p19 = position One Nine

    let before =
        emptyPuzzle
        |> addCell (unsolvedCell One One [ Two ])
        |> addCell (unsolvedCell One Nine [ Eight ])

    let expected = [ p11, Solved Two; p19, Solved Eight ]

    let actual = SingleDigit.rule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

[<Test>]
let hiddenPairTest () =
    let group = row (One)

    let combo: DigitCombination =
        { inside = [ One; Two ]
          outside = List.except [ One; Two ] AllDigits }

    let outsideDigits = List.except [ Three; Nine ] AllDigits

    let before =
        emptyPuzzle
        |> addCell (unsolvedCell One Three outsideDigits)
        |> addCell (unsolvedCell One Four outsideDigits)
        |> addCell (unsolvedCell One Five outsideDigits)
        |> addCell (unsolvedCell One Six outsideDigits)
        |> addCell (unsolvedCell One Seven outsideDigits)
        |> addCell (unsolvedCell One Eight outsideDigits)
        |> addCell (unsolvedCell One Nine outsideDigits)

    let retained =
        RetainPencils(Set.ofList [ Three; Nine ])

    let expected =
        [ (position One One), retained
          (position One Two), retained ]

    let actual =
        Tuple.hiddenPencils group combo (cellFinder before)

    Assert.AreEqual(expected, actual.changes)
