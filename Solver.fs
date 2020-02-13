module SudokuSolver.Solver

open SudokuSolver.Board

let possibleValues = Set.ofList [1 .. 9]

let possibleCellValues ((Index (row,col)) as index) board =
  let rowIndices = allRowIndices row
  let colIndices = allColIndices col
  let sectionIndices = allSectionIndices index
  [rowIndices;colIndices;sectionIndices]
  |> Seq.collect id
  |> Seq.map (fun i -> Map.find i board)
  |> Set.ofSeq
  |> Set.remove 0
  |> Set.difference possibleValues

let markCell index board =
  let cellVal = Map.find index board
  if cellVal = 0 then
    Undecided (possibleCellValues index board)
  else
    Value cellVal  

let toMarkedBoard board : Board = 
  board 
  |> Map.toSeq 
  |> Seq.map (fun (i,_) -> (i, markCell i board))
  |> Map.ofSeq


