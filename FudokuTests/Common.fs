module Fudoku.Tests.Common

open Fudoku.Domain
open Fudoku.Puzzle

let unsolvedCellRC r c ds =
    unsolvedCell (position r c) (Set.ofList ds)

let addCell cell map = Map.add cell.position cell map
