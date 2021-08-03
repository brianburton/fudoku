module Fudoku.PuzzleIO

open Fudoku.Domain
open System.Text.RegularExpressions

let stringToPuzzle source =
    let charToDigit c =
        match c with
        | '1' -> Some One
        | '2' -> Some Two
        | '3' -> Some Three
        | '4' -> Some Four
        | '5' -> Some Five
        | '6' -> Some Six
        | '7' -> Some Seven
        | '8' -> Some Eight
        | '9' -> Some Nine
        | _ -> None

    let createCell (pos, digit) =
        match digit with
        | Some d -> pos, solvedCell pos d
        | None -> pos, starterCell pos

    let filtered =
        Regex.Replace(source, "[^0123456789.]", "")

    let digits =
        filtered |> Seq.map charToDigit |> Seq.toList

    try
        let puzzle =
            List.zip AllPositions digits
            |> List.map createCell
            |> Map.ofList
        Ok puzzle
     with
        | e -> Error e
