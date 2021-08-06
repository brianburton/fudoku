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
let singleDigitTests () =
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
let singleCellPencilTests () =
    let group = row One

    let combo: DigitCombination =
        { inside = [ Two ]
          outside = List.except [ Two ] AllDigits }

    let outsideDigits = List.except [ Nine ] AllDigits

    let before =
        emptyPuzzle
        |> addCell (unsolvedCell One One outsideDigits)
        |> addCell (unsolvedCell One Two [ Nine ])
        |> addCell (unsolvedCell One Three outsideDigits)
        |> addCell (unsolvedCell One Four outsideDigits)
        |> addCell (unsolvedCell One Five outsideDigits)
        |> addCell (unsolvedCell One Six outsideDigits)
        |> addCell (unsolvedCell One Seven outsideDigits)
        |> addCell (unsolvedCell One Eight outsideDigits)
        |> addCell (unsolvedCell One Nine outsideDigits)

    let expected =
        [ (position One Two), Solved Nine ]

    let actual =
        Tuple.singleCellPencil group combo (cellFinder before)

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

[<Test>]
let cellsLinkedByDigitsTest () =
    Assert.AreEqual(true, Tuple.cellsLinkedByDigits [ (unsolvedCell Four Five [ One; Two ]) ] (Set.ofList [ One ]))

    Assert.AreEqual(
        true,
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two ])
              (unsolvedCell Four Six [ One; Two ]) ]
            (Set.ofList [ One; Two ])
    )

    Assert.AreEqual(
        true, // 7-> 5 -> 6 -> 7
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two ])
              (unsolvedCell Four Six [ Two; Three ])
              (unsolvedCell Four Seven [ One; Three ]) ]
            (Set.ofList [ One; Two; Three ])
    )

    Assert.AreEqual(
        true, // 8 -> 5 -> 6 -> 7 -> 8
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two; Nine ])
              (unsolvedCell Four Six [ Two; Three; Eight ])
              (unsolvedCell Four Seven [ Three; Four; Seven ])
              (unsolvedCell Four Eight [ One; Four; Three ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true, // 7 -> 8 -> 5 -> 6
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two; Nine ])
              (unsolvedCell Four Six [ One; Two; Eight ])
              (unsolvedCell Four Seven [ Three; Four; Seven ])
              (unsolvedCell Four Eight [ One; Four; Three ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true,
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two ])
              (unsolvedCell Four Six [ One; Two ])
              (unsolvedCell Four Seven [ Two; Three ]) ]
            (Set.ofList [ One; Two; Three ])
    )

    Assert.AreEqual(
        false,
        Tuple.cellsLinkedByDigits
            [ (unsolvedCell Four Five [ One; Two ])
              (unsolvedCell Four Six [ One; Two ])
              (unsolvedCell Four Seven [ Three; Four ])
              (unsolvedCell Four Eight [ Three; Four ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )
