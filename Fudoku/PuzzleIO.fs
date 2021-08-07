module Fudoku.PuzzleIO

open Fudoku.Domain
open Fudoku.Puzzle
open System.Text.RegularExpressions

let digitToString d =
    match d with
    | One -> "1"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"

let digitsToString ds =
    Set.toList ds
    |> List.map digitToString
    |> List.fold (fun acc s -> acc + s) ""

let positionToString pos =
    $"({pos.row},{pos.col})"

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

let solvedCellsToString cells =
    let cell c =
        match c with
        | { position = _; value = Answer d } -> digitToString d
        | _ -> " "

    let rec segment group str =
        match group with
        | a :: b :: c :: tail -> segment tail $"{str} {cell a} {cell b} {cell c} |"
        | _ -> str

    segment cells "|"

let unsolvedCellsToString cells =
    let cell c =
        let s = cellPencils c |> digitsToString
        let pad = 9 - s.Length
        let leftPad = pad / 2
        let rightPad = pad - leftPad
        sprintf "%*s%s%*s" leftPad "" s rightPad ""

    let rec segment group str =
        match group with
        | a :: b :: c :: tail -> segment tail $"{str} {cell a} {cell b} {cell c} |"
        | _ -> str

    segment cells "|"

let puzzleToString pz =
    let lookup = cellFinder pz

    let rowToString r =
        let group = List.map lookup r
        let solved = solvedCellsToString group
        let unsolved = unsolvedCellsToString group
        $"{solved}{unsolved}"

    let rowStrings =
        AllDigits
        |> List.map row
        |> List.map rowToString

    let solvedBorder = "+-------+-------+-------+"
    let unsolvedBorder = "+-------------------------------+-------------------------------+-------------------------------+"
    let combinedBorder = $"{solvedBorder}{unsolvedBorder}"
    let rec bordered rs ls =
        match rs with
        | a::b::c::tail -> bordered tail (ls @ [combinedBorder] @ [ a ; b ; c ])
        | _ -> ls

    (bordered rowStrings []) @ [ combinedBorder ]
    |> List.fold (fun c s -> $"{c}{s}\n") ""
