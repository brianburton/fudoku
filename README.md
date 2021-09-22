# Fudoku - Sudoku Puzzle Solver

This program is a learning project I used to try out functional programming in F#.
Sudoku is a great subject for learning projects since it is algorithm heavy and works with a well defined set of data types.
I've done solvers in other languages as well (C# and Kotlin) but enjoyed the F# solver the most.

## How it works

The main program accepts the name of a file containing a puzzle to be solved.
The puzzle file is simply a text file containing 81 characters.
Each character contains the known value in each cell.
This can be either a digit (1-9) or a place holder (period or 0).
Newlines can optionally be used to separate rows of the puzzle.

The puzzle is solved by applying one rule at a time to the current state of the puzzle.
Each rule attempts to create a list of cells to be changed.
The possible changes are:

- `Solved` - specifies the digit solution for a cell.
- `RemovePencils` - specifies a set of pencil digits (candidates) to remove from an unsolved cell.
- `RetainPencils` - specifies a set of pencil digits (candidates) to be retained in an unsolved cell.  All others will be removed.
- `AddPencils` - specifies a set of pencil digits (candidates) to be added to a cell.

Once any rule finds changes to be made those are applied to the puzzle and the process starts over.
This repeats until the puzzle has been solved or no rule finds any changes.

The `Rules` module contains all of the rules.
Each is just a function that accepts a function that can be used to look up the contents of any cell.

This isn't meant to be an especially efficient puzzle solver.
It's really just meant to be easy to write and understand.
In fact the rules themselves deliberately only find one possible set of changes even though multiple might be possible.
This is to make it easy to follow the logic of how the puzzle was solved.
An efficient solver would try to do the maximum amount of work at each stage.

## Rules

There are many more rules that could be added.
The current set is able to solve a wide variety of puzzles though.
I may add more over time.
Here are the currently implemented rules:

| Rule | Description |
|---|---|
| FixPencils | Looks for solved cells and removes their digits from adjacent cells. |
| SingleDigit | Looks for cells with only one pencil digit and sets that digit as the solution for the cell. |
| SingleCell | Looks for units (groups of adjacent cells) in which only one cell contains a certain digit and sets that digit as the solution of that cell. |
| SingleBox | Looks for a row or column in which a digit is only possible within a single box and removes that digit from the pencils of other cells in the box. |
| XYWing | A particular digit chain involving 3 cells with 2 pencils each. |
| Tuples | Hidden or naked pairs, triples, or quads. |
| Unique Rectangles | Looks for special cases to avoid non-unique solutions. |
| X-Wing | A two row/column fish. |
| DigitChain | Two color coloring of a chain of digits. |
| BUG | When only one unsolved cell contains 3 pencil digits and the rest contain 2. |
| Swordfish | A three row/column fish. |
| Jellyfish | A four row/column fish. |

## Reach Out

If you find this interesting or want to offer suggestions for improvement or new rules let me know!
