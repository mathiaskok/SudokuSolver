module SudokuSolver.Board

type Cell = 
  | Value of int
  | Undecided of int Set

type Index = Index of row:int * col:int

type Board = Map<Index,Cell>

let allRowIndices row =
  seq {1 .. 9}
  |> Seq.map (fun col -> Index (row,col))

let allColIndices col = 
  seq {1 .. 9}
  |> Seq.map (fun row -> Index (row,col))

let allSectionRowsRowsOrCols rowOrCol = 
  if rowOrCol <= 3 then
    seq {1 .. 3}
  else if rowOrCol <= 6 then
    seq {4 .. 6}
  else
    seq {7 .. 9}

let allSectionIndices (Index (rows,cols)) = seq {
  for row in (allSectionRowsRowsOrCols rows) do
    for col in (allSectionRowsRowsOrCols cols) do
      yield Index (row,col)
}