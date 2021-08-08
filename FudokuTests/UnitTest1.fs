module Fudoku.Tests.Misc

open NUnit.Framework
open Fudoku.Tests.Common
open Fudoku.Domain
open Fudoku.Puzzle

[<SetUp>]
let Setup () = ()

[<Test>]
let singleDigitTests () =
    let p11 = position One One
    let p19 = position One Nine

    let before =
        emptyPuzzle
        |> addCell (unsolvedCellRC One One [ Two ])
        |> addCell (unsolvedCellRC One Nine [ Eight ])

    let expected = [ p11, Solved Two; p19, Solved Eight ]

    let actual =
        Fudoku.SingleDigit.rule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

[<Test>]
let singleCellTests () =
    let outsideDigits = List.except [ Nine ] AllDigits

    let before =
        emptyPuzzle
        |> addCell (unsolvedCellRC One One outsideDigits)
        |> addCell (unsolvedCellRC One Two [ Nine ])
        |> addCell (unsolvedCellRC One Three outsideDigits)
        |> addCell (unsolvedCellRC One Four outsideDigits)
        |> addCell (unsolvedCellRC One Five outsideDigits)
        |> addCell (unsolvedCellRC One Six outsideDigits)
        |> addCell (unsolvedCellRC One Seven outsideDigits)
        |> addCell (unsolvedCellRC One Eight outsideDigits)
        |> addCell (unsolvedCellRC One Nine outsideDigits)

    let expected = [ (position One Two), Solved Nine ]

    let actual =
        Fudoku.SingleCell.rule (cellFinder before)

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
        |> addCell (unsolvedCellRC One Three outsideDigits)
        |> addCell (unsolvedCellRC One Four outsideDigits)
        |> addCell (unsolvedCellRC One Five outsideDigits)
        |> addCell (unsolvedCellRC One Six outsideDigits)
        |> addCell (unsolvedCellRC One Seven outsideDigits)
        |> addCell (unsolvedCellRC One Eight outsideDigits)
        |> addCell (unsolvedCellRC One Nine outsideDigits)

    let retained =
        RetainPencils(Set.ofList [ Three; Nine ])

    let expected =
        [ (position One One), retained
          (position One Two), retained ]

    let actual =
        Fudoku.Tuple.hiddenPencils group combo (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

[<Test>]
let cellsLinkedByDigitsTest () =
    Assert.AreEqual(true, Fudoku.Tuple.cellsLinkedByDigits [ (unsolvedCellRC Four Five [ One; Two ]) ] (Set.ofList [ One ]))

    Assert.AreEqual(
        true,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ]) ]
            (Set.ofList [ One; Two ])
    )

    Assert.AreEqual(
        true, // 7-> 5 -> 6 -> 7
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ Two; Three ])
              (unsolvedCellRC Four Seven [ One; Three ]) ]
            (Set.ofList [ One; Two; Three ])
    )

    Assert.AreEqual(
        true, // 8 -> 5 -> 6 -> 7 -> 8
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two; Nine ])
              (unsolvedCellRC Four Six [ Two; Three; Eight ])
              (unsolvedCellRC Four Seven [ Three; Four; Seven ])
              (unsolvedCellRC Four Eight [ One; Four; Three ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true, // 7 -> 8 -> 5 -> 6
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two; Nine ])
              (unsolvedCellRC Four Six [ One; Two; Eight ])
              (unsolvedCellRC Four Seven [ Three; Four; Seven ])
              (unsolvedCellRC Four Eight [ One; Four; Three ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ])
              (unsolvedCellRC Four Seven [ Two; Three ]) ]
            (Set.ofList [ One; Two; Three ])
    )

    Assert.AreEqual(
        false,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ])
              (unsolvedCellRC Four Seven [ Three; Four ])
              (unsolvedCellRC Four Eight [ Three; Four ]) ]
            (Set.ofList [ One; Two; Three; Four ])
    )
