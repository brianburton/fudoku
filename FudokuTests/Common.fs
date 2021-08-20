module Fudoku.Tests.Common

open FSharpx.Collections
open Fudoku.Domain
open Fudoku.Puzzle

let dig x =
    match x with
    | 1 -> One
    | 2 -> Two
    | 3 -> Three
    | 4 -> Four
    | 5 -> Five
    | 6 -> Six
    | 7 -> Seven
    | 8 -> Eight
    | 9 -> Nine
    | _ -> invalidArg "x" "not a valid digit"

let digList digits = digits |> List.map dig
let digSet digits = digits |> digList |> Set.ofList

let pos (a, b) = position (dig a) (dig b)
let posList list = list |> List.map pos

let bulkApplyChange positions change puzzle =
    let changes = positions |> List.map (fun p -> p, change)
    puzzle |> applyRuleResults changes

let bulkAddPencils positions digits puzzle =
    let change = AddPencils(digits |> digSet)
    puzzle |> bulkApplyChange positions change

let bulkRemovePencils positions digits puzzle =
    let change = RemovePencils(digits |> digSet)
    puzzle |> bulkApplyChange positions change

let bulkRetainPencils positions digits puzzle =
    let change = RetainPencils(digits |> digSet)
    puzzle |> bulkApplyChange positions change

let unsolvedCellRC r c ds = unsolvedCell (position r c) (Set.ofList ds)

let solvedCellRC r c d = solvedCell (position r c) d

let addCell cell map = PersistentHashMap.add cell.position cell map
