#load "Combinations.fs"
#load "Permutations.fs"
#load "Types.fs"
#load "Solver.fs"

open Sudoku.Solver

let strGrid = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
let solver = Solver(strGrid)
solver.Solve() |> ignore
