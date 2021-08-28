module Fudoku.Tests.FastSet

open NUnit.Framework
open Fudoku

let EmptySet: FastSet<int> = FastSet.empty ()
let EmptyList: List<int> = []

let sorted (set: FastSet<int>) = set |> FastSet.toList |> List.sort

let set xs = xs |> FastSet.ofSeq


[<Test>]
let ``basic creation functions`` () =
    Assert.AreEqual(FastSet.singleton 3, EmptySet |> FastSet.add 3)
    Assert.AreEqual([ 4; 7 ], [ 7; 4; 7 ] |> FastSet.addAll EmptySet |> sorted)
    Assert.AreEqual([ 4; 7 ], [ 7; 4; 4 ] |> FastSet.ofSeq |> sorted)

[<Test>]
let ``size ops`` () =
    Assert.AreEqual(0, FastSet.length EmptySet)
    Assert.AreEqual(true, FastSet.isEmpty EmptySet)
    Assert.AreEqual(3, FastSet.length (set [1;2;3]))
    Assert.AreEqual(false, FastSet.isEmpty (set [1;2;3]))

[<Test>]
let ``removal functions`` () =
    Assert.AreEqual(EmptySet, FastSet.remove 3 EmptySet)
    Assert.AreEqual(EmptySet, FastSet.singleton 3 |> FastSet.remove 3)
    Assert.AreEqual(FastSet.singleton 3, FastSet.singleton 3 |> FastSet.remove 5)
    Assert.AreEqual(FastSet.singleton 3, [ 3; 5 ] |> FastSet.ofSeq |> FastSet.remove 5)

    Assert.AreEqual(
        FastSet.singleton 3,
        [ 3; 5; 7 ]
        |> FastSet.ofSeq
        |> FastSet.except [ 5; 7 ]
    )

[<Test>]
let ``usual set operations`` () =
    Assert.AreEqual(EmptySet, FastSet.intersect EmptySet EmptySet)
    Assert.AreEqual(EmptySet, FastSet.intersect EmptySet (set [ 1; 3 ]))
    Assert.AreEqual(EmptySet, FastSet.intersect (set [ 1; 3 ]) EmptySet)

    Assert.AreEqual(set [ 1; 3 ], FastSet.intersect (set [ 1; 3 ]) (set [ 1; 3; 5 ]))
    Assert.AreEqual(set [ 1; 3; 5 ], FastSet.union (set [ 1; 3 ]) (set [ 3; 5 ]))

    Assert.AreEqual(set [ 1; 3; 5 ], FastSet.difference (set [ 1; 3; 5 ]) EmptySet)
    Assert.AreEqual(set [ 5 ], FastSet.difference (set [ 1; 3; 5 ]) (set [ 1; 3 ]))

[<Test>]
let ``overlaps`` () =
    Assert.AreEqual(false, FastSet.overlaps EmptySet EmptySet)
    Assert.AreEqual(false, FastSet.overlaps (set [ 1 ]) EmptySet)
    Assert.AreEqual(false, FastSet.overlaps EmptySet (set [ 1 ]))
    Assert.AreEqual(false, FastSet.overlaps (set [ 3 ]) (set [ 1 ]))

    Assert.AreEqual(true, FastSet.overlaps (set [ 1; 3 ]) (set [ 1 ]))
    Assert.AreEqual(true, FastSet.overlaps (set [ 1 ]) (set [ 3; 1 ]))
    Assert.AreEqual(true, FastSet.overlaps (set [ 1; 3 ]) (set [ 3; 1 ]))

[<Test>]
let ``containsAll`` () =
    Assert.AreEqual(true, FastSet.containsAll EmptySet EmptyList)
    Assert.AreEqual(true, FastSet.containsAll (set [ 1 ]) EmptyList)
    Assert.AreEqual(false, FastSet.containsAll EmptySet [ 1 ])
    Assert.AreEqual(false, FastSet.containsAll (set [ 3 ]) [ 1 ])

    Assert.AreEqual(true, FastSet.containsAll (set [ 1; 3 ]) [ 1 ])
    Assert.AreEqual(false, FastSet.containsAll (set [ 1 ]) [ 3; 1 ])
    Assert.AreEqual(true, FastSet.containsAll (set [ 1; 3 ]) [ 3; 1 ])

[<Test>]
let ``containsAny`` () =
    Assert.AreEqual(false, FastSet.containsAny EmptySet EmptyList)
    Assert.AreEqual(false, FastSet.containsAny (set [ 1 ]) EmptyList)
    Assert.AreEqual(false, FastSet.containsAny EmptySet [ 1 ])
    Assert.AreEqual(false, FastSet.containsAny (set [ 3 ]) [ 1 ])

    Assert.AreEqual(true, FastSet.containsAny (set [ 1; 3 ]) [ 1 ])
    Assert.AreEqual(true, FastSet.containsAny (set [ 1 ]) [ 3; 1 ])
    Assert.AreEqual(true, FastSet.containsAny (set [ 1; 3 ]) [ 3; 1 ])

[<Test>]
let ``monadic operations`` () =
    Assert.AreEqual(set [ 10; 15 ], set [ 0; 5 ] |> FastSet.map (fun x -> x + 10))

    Assert.AreEqual(
        set [ 5; 10; 15 ],
        set [ 0; 5 ]
        |> FastSet.bind (fun x -> set ([ x + 5; x + 10 ]))
    )


[<Test>]
let ``filtering operations`` () =
    Assert.AreEqual(EmptySet, set [ 10; 15 ] |> FastSet.filter (fun x -> x < 10))
    Assert.AreEqual(set [ 15 ], set [ 10; 15 ] |> FastSet.filter (fun x -> x > 10))

    Assert.AreEqual(
        EmptyList,
        [ 10; 15 ]
        |> List.filter (FastSet.toFilter EmptySet)
    )

    Assert.AreEqual(
        [ 10 ],
        [ 10; 15 ]
        |> List.filter (FastSet.toFilter (set [ 10 ]))
    )

[<Test>]
let ``supersets`` () =
    Assert.AreEqual(true, FastSet.isSuperset EmptySet EmptySet)
    Assert.AreEqual(false, FastSet.isSuperset EmptySet (set [ 10 ]))
    Assert.AreEqual(true, FastSet.isSuperset (set [ 10 ]) EmptySet)

    Assert.AreEqual(true, FastSet.isSuperset (set [ 10 ]) (set [ 10 ]))
    Assert.AreEqual(false, FastSet.isProperSuperset (set [ 10 ]) (set [ 10 ]))

    Assert.AreEqual(true, FastSet.isSuperset (set [ 10; 11 ]) (set [ 10 ]))
    Assert.AreEqual(true, FastSet.isProperSuperset (set [ 10; 11 ]) (set [ 10 ]))

    Assert.AreEqual(false, FastSet.isSuperset (set [ 10 ]) (set [ 10; 11 ]))
    Assert.AreEqual(false, FastSet.isProperSuperset (set [ 10 ]) (set [ 10; 11 ]))

[<Test>]
let ``miscellaneous ops`` () =
    Assert.AreEqual(10, FastSet.head (set [10]))

    Assert.AreEqual(true, FastSet.equals EmptySet EmptySet)
    Assert.AreEqual(true, FastSet.equals (set [10]) (set [10]))
    Assert.AreEqual(false, FastSet.equals EmptySet (set [10]))
    Assert.AreEqual(false, FastSet.equals EmptySet (set [10]))

    Assert.AreEqual(false, FastSet.notEquals EmptySet EmptySet)
    Assert.AreEqual(false, FastSet.notEquals (set [10]) (set [10]))
    Assert.AreEqual(true, FastSet.notEquals EmptySet (set [10]))
    Assert.AreEqual(true, FastSet.notEquals EmptySet (set [10]))
