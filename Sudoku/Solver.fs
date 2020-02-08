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

    let findSinglesSimple (cands: seq<Cell>) : FindResult =
        let nsolved =
            cands
            |> Seq.groupBy (fun (c: Cell) -> c.Pos)
            |> Seq.filter (fun (xs: (Pos * seq<Cell>)) ->
                           xs
                           |> snd
                           |> Seq.length
                           |> (=) 1)
            |> Seq.map (snd >> Seq.head)

        { Solved = nsolved; Eliminated = Seq.empty; }

    type Solver(grid: string) =
        let mutable _grid : seq<Cell> = Seq.empty
        let mutable _candidates : seq<Cell> = Seq.empty
        let mutable _solved : Map<Pos, Cell> = Map.empty
        let mutable _unsolved : Set<Pos> = Set.empty

        let getCandidates (solved: seq<Cell>, unsolvedSet: Set<Pos>) : seq<Cell> =
            let gen (pos: Pos) = seq {
                for n in [1..9] do
                    yield Cell(n, pos.Row, pos.Column)
            }

            let unsolved (pos: Pos) =
                if unsolvedSet.Contains(pos) then (gen pos)
                else Seq.empty

            let filterFun (cell: Cell) : bool =
                solved
                |> Seq.exists (fun (cell2: Cell) ->
                               cell.Pos.Sees(cell2.Pos) && cell.Value = cell2.Value)

            seq {
                for row in [1..9] do
                for col in [1..9] do
                yield! (unsolved (Pos(row, col)))
            }
            |> Seq.filter (filterFun >> not)

        do
            _grid <- strToGrid grid
            let (solved, unsolved) =
                _grid
                |> List.ofSeq
                |> List.partition (fun (cell: Cell) ->
                                   match cell.Value with
                                   | CellValue.Unsolved -> false
                                   | CellValue.Value num -> true)

            _solved <-
                solved
                |> Seq.map (fun cell -> (cell.Pos, cell))
                |> Map.ofSeq

            _unsolved <-
                unsolved
                |> Seq.map (fun cell -> cell.Pos)
                |> Set.ofSeq

            _candidates <-
                getCandidates(Seq.ofList solved, _unsolved)

        member this.Grid = _grid
        member this.Candidates = _candidates

        member this.UpdateCandidates(eliminated: seq<Cell>) : seq<Cell> =
            printfn "UpdateCandidates"

            let ncandidates =
                eliminated
                |> Seq.fold (fun (acc: seq<Cell>) (cell: Cell) ->
                             acc
                             |> Seq.filter (fun (cell2: Cell) ->
                                            not (cell.Pos = cell2.Pos && cell.Value = cell2.Value)))
                             _candidates

            printfn "Candidates length %d" (Seq.length ncandidates)
            _candidates <- ncandidates
            _candidates

        member this.UpdateGrid(newSolved: seq<Cell>) : seq<Cell> =
            printfn "UpdateGrid"

            _solved <-
                newSolved
                |> Seq.fold (fun (acc: Map<Pos, Cell>) (cell: Cell) ->
                             acc
                             |> Map.add cell.Pos cell)
                             _solved

            _unsolved <-
                newSolved
                |> Seq.fold (fun (acc: Set<Pos>) (cell: Cell) ->
                             acc
                             |> Set.remove cell.Pos)
                             _unsolved

            let ncandidates =
                newSolved
                |> Seq.fold (fun (acc: seq<Cell>) (cell: Cell) ->
                             acc
                             |> Seq.filter (fun (cell2: Cell) ->
                                            not ((cell.Pos = cell2.Pos || cell.Pos.Sees(cell2.Pos))
                                                 && cell.Value = cell2.Value)))
                             _candidates

            printfn "Candidates length %d" (Seq.length ncandidates)
            _candidates <- ncandidates
            _candidates

        member this.Solve() : bool =
            let cnt = Seq.length _candidates

            printfn "Start from %d candidates" cnt

            let finders : List<seq<Cell> -> FindResult> = [
                findSinglesSimple
                ]

            let rec findLoop finders candidates =
                match finders with
                | [] ->
                    // printfn "Empty?"
                    (Seq.empty, Seq.empty)
                | f::fs ->
                    // printfn "Try fun %A" f
                    match f candidates with
                        | { Solved = solved; Eliminated = eliminated } when Seq.isEmpty solved && Seq.isEmpty eliminated ->
                            // no progress, still possible to try other finders
                            // printfn "No progress in inner loop"
                            findLoop fs candidates
                        | { Solved = solved; Eliminated = eliminated } ->
                            // printfn "Progress in inner loop"
                            (solved, eliminated)

            let rec solve candidates =
                printfn "Solve loop with %d candidates" (Seq.length candidates)

                match findLoop finders candidates with
                    | (solved, eliminated) when Seq.isEmpty solved && Seq.isEmpty eliminated ->
                        // no progress, finders exhausted
                        false
                    | (solved, eliminated) when not (Seq.isEmpty solved) ->
                        printfn "Found solved? %A" solved
                        let ncandidates = this.UpdateGrid solved
                        solve ncandidates
                    | (solved, eliminated) when not (Seq.isEmpty eliminated) ->
                        printfn "Found eliminated? %A" eliminated
                        let ncandidates = this.UpdateCandidates eliminated
                        solve ncandidates
                    | _ -> false

            let solved = solve _candidates
            false
