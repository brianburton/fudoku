module Fudoku.Tests.Misc

open NUnit.Framework
open Fudoku.Tests.Common
open Fudoku.Domain
open Fudoku.Puzzle
open Fudoku.Utils

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

    let actual = Fudoku.SingleDigit.rule (cellFinder before)

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

    let actual = Fudoku.SingleCell.rule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

[<Test>]
let hiddenPairTest () =
    let group = row (One)

    let combo: Combination<Digit> = { inside = [ One; Two ]; outside = List.except [ One; Two ] AllDigits }

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

    let retained = RetainPencils(FastSet.ofSeq [ Three; Nine ])

    let expected = [ (position One One), retained; (position One Two), retained ]

    let actual = Fudoku.Tuple.hiddenPairsRule (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

[<Test>]
let cellsLinkedByDigitsTest () =
    Assert.AreEqual(true, Fudoku.Tuple.cellsLinkedByDigits [ (unsolvedCellRC Four Five [ One; Two ]) ] (FastSet.ofSeq [ One ]))

    Assert.AreEqual(
        true,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ]) ]
            (FastSet.ofSeq [ One; Two ])
    )

    Assert.AreEqual(
        true, // 7-> 5 -> 6 -> 7
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ Two; Three ])
              (unsolvedCellRC Four Seven [ One; Three ]) ]
            (FastSet.ofSeq [ One; Two; Three ])
    )

    Assert.AreEqual(
        true, // 8 -> 5 -> 6 -> 7 -> 8
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two; Nine ])
              (unsolvedCellRC Four Six [ Two; Three; Eight ])
              (unsolvedCellRC Four Seven [ Three; Four; Seven ])
              (unsolvedCellRC Four Eight [ One; Four; Three ]) ]
            (FastSet.ofSeq [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true, // 7 -> 8 -> 5 -> 6
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two; Nine ])
              (unsolvedCellRC Four Six [ One; Two; Eight ])
              (unsolvedCellRC Four Seven [ Three; Four; Seven ])
              (unsolvedCellRC Four Eight [ One; Four; Three ]) ]
            (FastSet.ofSeq [ One; Two; Three; Four ])
    )

    Assert.AreEqual(
        true,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ])
              (unsolvedCellRC Four Seven [ Two; Three ]) ]
            (FastSet.ofSeq [ One; Two; Three ])
    )

    Assert.AreEqual(
        false,
        Fudoku.Tuple.cellsLinkedByDigits
            [ (unsolvedCellRC Four Five [ One; Two ])
              (unsolvedCellRC Four Six [ One; Two ])
              (unsolvedCellRC Four Seven [ Three; Four ])
              (unsolvedCellRC Four Eight [ Three; Four ]) ]
            (FastSet.ofSeq [ One; Two; Three; Four ])
    )

[<Test>]
let setMaps () =
    let emptySet: FastSet<Digit> = FastSet.empty ()

    let m =
        SetMap.empty ()
        |> SetMap.add One Two
        |> SetMap.add One Three
        |> SetMap.add Two Four

    Assert.AreEqual((FastSet.ofSeq [ Two; Three ]), (SetMap.get One m))
    Assert.AreEqual((FastSet.ofSeq [ Four ]), (SetMap.get Two m))
    Assert.AreEqual(emptySet, (SetMap.get Three m))

    let m2 = SetMap.remove One Two m
    Assert.AreEqual((FastSet.ofSeq [ Three ]), (SetMap.get One m2))
    Assert.AreEqual((FastSet.ofSeq [ Four ]), (SetMap.get Two m2))

    let m3 = SetMap.remove One Two m2
    Assert.AreEqual((FastSet.ofSeq [ Three ]), (SetMap.get One m3))
    Assert.AreEqual((FastSet.ofSeq [ Four ]), (SetMap.get Two m3))

    let m4 = SetMap.remove One Three m3
    Assert.AreEqual(emptySet, (SetMap.get One m4))
    Assert.AreEqual((FastSet.ofSeq [ Four ]), (SetMap.get Two m4))

    let splitter x = (String.length x, x)

    let x =
        [ "a"; "ab"; "abc"; "cd"; "efg" ]
        |> List.fold (SetMap.folder splitter) (SetMap.empty ())

    Assert.AreEqual(FastSet.ofSeq [ "ab"; "cd" ], SetMap.get 2 x)

    let y =
        [ "a"; "ab"; "abc"; "cd"; "efg" ]
        |> SetMap.ofList splitter

    Assert.AreEqual(FastSet.ofSeq [ "ab"; "cd" ], SetMap.get 2 y)

    let z =
        [ "a"; "ab"; "abc"; "cd"; "efg" ]
        |> List.map splitter
        |> SetMap.ofPairs

    Assert.AreEqual(FastSet.ofSeq [ "abc"; "efg" ], SetMap.get 3 z)
