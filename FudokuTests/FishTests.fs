module Fudoku.Tests.FishTests

open NUnit.Framework
open Fudoku.Tests.Common
open Fudoku.Domain
open Fudoku.Puzzle
open Fudoku.Fish


[<Test>]
let ``find adjacent positions in group`` () =
    let empty: Position list = []
    let a = (position One One)
    let b = (position One Two)
    let c = (position One Three)
    let d = (position One Four)
    let e = (position One Five)

    let skip = Set.empty
    Assert.AreEqual(empty, (empty |> adjacentPositionsInGroup skip a))

    let group = [ a ]
    Assert.AreEqual(empty, (group |> adjacentPositionsInGroup skip a))

    let group = [ a; b ]

    Assert.AreEqual([ b ], (group |> adjacentPositionsInGroup skip a))
    Assert.AreEqual([ a ], (group |> adjacentPositionsInGroup skip b))

    let group = [ a; b; c ]

    Assert.AreEqual([ b ], (group |> adjacentPositionsInGroup skip a))
    Assert.AreEqual([ a; c ], (group |> adjacentPositionsInGroup skip b))
    Assert.AreEqual([ b ], (group |> adjacentPositionsInGroup skip c))

    let group = [ a; b; c; d ]

    Assert.AreEqual([ b ], (group |> adjacentPositionsInGroup skip a))
    Assert.AreEqual([ a; c ], (group |> adjacentPositionsInGroup skip b))
    Assert.AreEqual([ b; d ], (group |> adjacentPositionsInGroup skip c))
    Assert.AreEqual([ c ], (group |> adjacentPositionsInGroup skip d))

    let group = [ a; b; c; d; e ]
    Assert.AreEqual([ b ], (group |> adjacentPositionsInGroup skip a))
    Assert.AreEqual([ a; c ], (group |> adjacentPositionsInGroup skip b))
    Assert.AreEqual([ b; d ], (group |> adjacentPositionsInGroup skip c))
    Assert.AreEqual([ c; e ], (group |> adjacentPositionsInGroup skip d))
    Assert.AreEqual([ d ], (group |> adjacentPositionsInGroup skip e))

    let skip = Set.ofList [ b; c ]
    Assert.AreEqual(empty, (group |> adjacentPositionsInGroup skip a))
    Assert.AreEqual([ a ], (group |> adjacentPositionsInGroup skip b))
    Assert.AreEqual([ d ], (group |> adjacentPositionsInGroup skip c))
    Assert.AreEqual([ e ], (group |> adjacentPositionsInGroup skip d))
    Assert.AreEqual([ d ], (group |> adjacentPositionsInGroup skip e))

[<Test>]
let ``all positions have path to all others`` () =
    let a1 = (position One One)
    let a2 = (position One Two)
    let b1 = (position Two One)
    let b2 = (position Two Two)
    let b3 = (position Two Three)
    let c1 = (position Three One)
    let c3 = (position Three Three)

    let group = [ a1 ]
    Assert.IsTrue(allPositionsLinked group)

    let group = [ a1; a2 ]

    Assert.IsTrue(allPositionsLinked group)

    let group = [ a1; a2; b1 ]

    Assert.IsFalse(allPositionsLinked group)

    let group = [ a1; a2; b1; b2 ]

    Assert.IsTrue(allPositionsLinked group)

    let group = [ a1; a2; b1; b2; b3; c1 ]

    Assert.IsFalse(allPositionsLinked group)

    let group = [ a1; a2; b1; b2; b3; c1; c3 ]

    Assert.IsTrue(allPositionsLinked group)

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
