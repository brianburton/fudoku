module Fudoku.Tests.FishTests

open NUnit.Framework
open Fudoku.Domain
open Fudoku.Fish
open Fudoku

[<Test>]
let ``findCandidates test`` () =
    let posMap =
        [ (One, One)
          (One, Three)
          (One, Five)
          (Two, One)
          (Two, Six)
          (Three, One)
          (Three, Three)
          (Four, One)
          (Four, Three)
          (Four, Five)
          (Four, Six)
          (Five, One)
          (Five, Three)
          (Five, Five)
          (Eight, One)
          (Eight, Three)
          (Eight, Five) ]
        |> SetMap.ofPairs

    let answer = findCandidates 3 posMap |> Seq.toList

    Assert.AreEqual(
        [ ([ One; Three; Five ], [ One; Three; Five ])
          ([ One; Three; Eight ], [ One; Three; Five ])
          ([ One; Five; Eight ], [ One; Three; Five ])
          ([ Three; Five; Eight ], [ One; Three; Five ]) ],
        answer
    )

    ()

let ``valid 2 fish arrangements`` () =
    let a1 = (position One One)
    let a2 = (position One Two)
    let b1 = (position Two One)
    let b2 = (position Two Two)

    let dimensions = [ One; Two ]

    let group = [ a1 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b2 ]
    Assert.IsTrue(isValidFish group dimensions dimensions)

let ``valid 3 fish arrangements`` () =
    let a1 = (position One One)
    let a2 = (position One Two)
    let a3 = (position One Three)
    let b1 = (position Two One)
    let b2 = (position Two Two)
    let b3 = (position Two Three)
    let c1 = (position Three One)
    let c2 = (position Three Two)
    let c3 = (position Three Three)

    let dimensions = [ One; Two; Three ]

    let group = [ a1 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b2 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b2; b3; c1 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b2; b3; c3 ]
    Assert.IsFalse(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b2; b3; c1; c3 ]
    Assert.IsTrue(isValidFish group dimensions dimensions)

    let group = [ a1; a2; a3; b1; b2; b3; c1; c2 ]
    Assert.IsTrue(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b1; b3; c2; c3 ]
    Assert.IsTrue(isValidFish group dimensions dimensions)

    let group = [ a1; a2; b2; b3; c1; c3 ]
    Assert.IsTrue(isValidFish group dimensions dimensions)
