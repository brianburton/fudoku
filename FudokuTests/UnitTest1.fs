module Fudoku.Tests.Misc

open NUnit.Framework
open Fudoku.Tests.Common
open Fudoku.Domain
open Fudoku.Puzzle
open Fudoku

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

    let actual = Tuple.solveTuple 2 (cellFinder before)

    Assert.AreEqual(expected, actual.changes)

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
