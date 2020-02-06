// Copyright (c) 2020 Jani J. Hakala <jjhakala@gmail.com>, Finland
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Affero General Public License as
//  published by the Free Software Foundation, version 3 of the
//  License.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Affero General Public License for more details.
//
//  You should have received a copy of the GNU Affero General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
namespace Sudoku

module Solver =
    open Types

    let strToGrid (grid: seq<char>) : seq<Cell> =
        let charToInt x = int x - int '0'
        // grid |> Seq.iteri (fun i x -> printfn "%d %c" i x)
        grid
        |> Seq.mapi (fun i x ->
                     let r = (i / 9) + 1
                     let c = (i % 9) + 1
                     Cell(charToInt x, r, c))

    type Solver(grid: string) =
        let mutable _grid : seq<Cell> = Seq.empty
        let mutable _candidates : seq<Cell> = Seq.empty
        let mutable _solved : Map<Pos, Cell> = Map.empty

        let getCandidates (solved: Map<Pos, Cell>) : seq<Cell> =
            let gen (pos: Pos) = seq {
                for n in [1..9] do
                    yield Cell(n, pos.Row, pos.Column)
            }

            let unsolved (pos: Pos) =
                if (not (solved.ContainsKey pos)) then (gen pos)
                else Seq.empty

            seq {
                for row in [1..9] do
                for col in [1..9] do
                yield! (unsolved (Pos(row, col)))
            }

        do
            _grid <- strToGrid grid
            _solved <-
                     _grid
                     |> List.ofSeq
                     |> List.partition (fun (cell: Cell) ->
                                        match cell.Value with
                                        | CellValue.Unsolved -> false
                                        | CellValue.Value num -> true)
                    |> fst
                    |> Seq.map (fun cell -> (cell.Pos, cell))
                    |> Map.ofSeq

            _candidates <- getCandidates _solved

        member this.Grid = _grid
        member this.Candidates = _candidates

        member this.RemoveConflicting(cells: seq<Cell>) =
            let filterFun (cell: Cell) : bool =
                _grid
                |> Seq.exists (fun (cell2: Cell) ->
                               (cell.Pos = cell2.Pos && cell2.Value <> CellValue.Unsolved)
                               || (cell.Pos.Sees(cell2.Pos) && cell.Value = cell2.Value))

            printfn "Old count: %d" (Seq.length _candidates)

            _candidates <- _candidates
                |> Seq.filter (filterFun >> not)

            printfn "Removed any? %d" (Seq.length _candidates)
            ignore

        member this.Solve() : bool =
            let (solved, unsolved) =
                _grid
                |> List.ofSeq
                |> List.partition (fun (cell: Cell) ->
                                   match cell.Value with
                                   | CellValue.Unsolved -> false
                                   | CellValue.Value num -> true)

            let unsolvedMap =
                unsolved
                |> Seq.map (fun cell -> (cell.Pos, cell))
                |> dict

            let nsolved =
                solved
                |> Seq.ofList

            let ncandidates =
                _candidates
                |> Seq.filter (fun (cell: Cell) ->
                               unsolvedMap.ContainsKey cell.Pos)

            let removed =
                solved
                |> Seq.ofList
                |> this.RemoveConflicting
                |> ignore

            false
