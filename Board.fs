module SudokuSolver.Board

type Cell = 
  | Value of int
  | Undecided of int Set

type Index = Index of row:int*col:int

type Board = Map<Index,Cell>

let numberOfCells = 9*9

let allRowIndices row =
  seq {0 .. 8}
  |> Seq.map (fun col -> Index (row,col))

let allColIndices col = 
  seq {0 .. 8}
  |> Seq.map (fun row -> Index (row,col))

let allSectionRowsRowsOrCols rowOrCol = 
  if rowOrCol <= 2 then
    seq {0 .. 2}
  else if rowOrCol <= 5 then
    seq {3 .. 5}
  else
    seq {6 .. 8}

let allSectionIndices (Index (rows,cols)) = seq {
  for row in (allSectionRowsRowsOrCols rows) do
    for col in (allSectionRowsRowsOrCols cols) do
      yield Index (row,col)
}