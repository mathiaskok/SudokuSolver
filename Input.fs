module SudokuSolver.Input

open SudokuSolver.Board

let fromLinear i = 
  let col = i / 9
  let row = i % 9
  Index (row,col)

let toUnmarkedBoard problem =
  problem
  |> Seq.mapi (fun i v -> (fromLinear i, v))
  |> Map.ofSeq