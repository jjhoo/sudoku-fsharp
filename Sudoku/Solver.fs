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
    open Combinations
    open Permutations
    open Types

    let boxSets (cands: seq<Cell>) : seq<seq<Cell>> =
        seq {
            for row in [1..3] do
            for col in [1..3] do
            let box = newBox row col
            let set = cands |> Seq.filter (fun (cell: Cell) -> box = cell.Pos.Box)
            if not (Seq.isEmpty set)
            then yield set
        }

    let rowSets (cands: seq<Cell>) : seq<seq<Cell>> =
        seq {
            for row in [1..9] do
            let set = cands |> Seq.filter (fun (cell: Cell) -> row = cell.Pos.Row)
            if not (Seq.isEmpty set)
            then yield set
        }

    let colSets (cands: seq<Cell>) : seq<seq<Cell>> =
        seq {
            for col in [1..9] do
            let set = cands |> Seq.filter (fun (cell: Cell) -> col = cell.Pos.Column)
            if not (Seq.isEmpty set)
            then yield set
        }

    let allSets cands =
        [rowSets; colSets; boxSets;]
        |> Seq.map (fun f -> f cands)
        |> Seq.concat

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

    let findHiddenSingles (cands: seq<Cell>) : FindResult =
        let setSingles (cands: seq<Cell>) (itemFun: Cell -> 'a) =
            cands
            |> Seq.groupBy (fun (c: Cell) -> (itemFun c, c.Value))
            |> Seq.filter (fun (xs: _ * seq<Cell>) ->
                           xs
                           |> snd
                           |> Seq.length
                           |> (=) 1)
            |> Seq.map (snd >> Seq.head)

        let rowSingles = setSingles cands (fun (c: Cell) -> c.Pos.Row)
        let colSingles = setSingles cands (fun (c: Cell) -> c.Pos.Column)
        let boxSingles = setSingles cands (fun (c: Cell) -> c.Pos.Box)

        let solved =
            rowSingles
            |> Seq.append colSingles
            |> Seq.append boxSingles
            |> Seq.distinct

        { Solved = solved; Eliminated = Seq.empty; }

    let findSets (k: int) (pmatch: Set<int> -> Set<int> -> bool) (pelim: Set<Pos> -> Set<int> -> Cell -> bool) (cands: seq<Cell>) : FindResult =
        let cellNumbersSet : (seq<Cell> -> Map<Pos, Set<int>>) =
            let setF =
                Seq.map (fun (cell: Cell) -> cell.Num())
                >> Set.ofSeq

            Seq.groupBy (fun (c: Cell) -> c.Pos)
            >> Seq.map (fun (xs: (Pos * seq<Cell>)) ->
                        let (p, cells) = xs
                        (p, setF cells))
            >> Map.ofSeq

        let find (k: int) (nset: Set<int>) (set: seq<Cell>) : seq<Cell> =
            let cmap = cellNumbersSet set
            let n = Seq.length nset
            let numberArray : int [] = Set.toArray nset

            let cmapKeys cmap =
                cmap
                |> Map.toSeq
                |> Seq.map fst
                |> Set.ofSeq

            let tmp (cset: Set<int>) : seq<Cell> =
                let matching =
                    cmap
                    |> Map.filter (fun (p: Pos) (nset: Set<int>) -> pmatch nset cset)

                // printfn "matching %d %d %A" k (Seq.length matching) matching
                if Seq.length matching = k
                then Seq.filter (pelim (cmapKeys matching) cset) set
                else Seq.empty

            Combinations(n, k).toSeq
            |> Seq.map (Array.map (Array.get numberArray)
                        >> Set.ofSeq
                        >> tmp)
            |> Seq.concat
            |> Seq.distinct

        let findAll cands =
            let f set =
                let nset =
                    set
                    |> Seq.map (fun (cell: Cell) -> cell.Num())
                    |> Set.ofSeq

                // printfn "Find sets (n, k) = (%d, %d)" (Seq.length nset) k

                if Seq.length nset > k then (find k nset set)
                else Seq.empty

            allSets cands
            |> Seq.map f
            |> Seq.concat
            |> Seq.distinct
            |> Seq.sort

        let eliminated = findAll cands
        { Solved = Seq.empty; Eliminated = eliminated; }

    let findNakedSets (k: int) (cands: seq<Cell>) : FindResult =
        // printfn "findNakedSets %d %A" k cands

        let pelim (poss: Set<Pos>) (cset: Set<int>) (cell: Cell) : bool =
            let n = cell.Num()
            // printfn "pelim %d %A %A %A" n cell poss cset
            not (Set.contains cell.Pos poss) && Set.contains n cset

        let pmatch (nset: Set<int>) (cset: Set<int>) : bool =
            // printfn "pmatch %A %A %A %A" nset cset (Set.isSubset nset cset) (nset = cset)
            Set.isSubset nset cset

        findSets k pmatch pelim cands

    let findHiddenSets (k: int) (cands: seq<Cell>) : FindResult =
        // printfn "findHiddenSets %d %A" k cands

        let pelim (poss: Set<Pos>) (cset: Set<int>) (cell: Cell) : bool =
            let n = cell.Num()
            Set.contains cell.Pos poss && not (Set.contains n cset)

        let pmatch (nset: Set<int>) (cset: Set<int>) : bool =
            Seq.length (Set.intersect nset cset) > 0

        findSets k pmatch pelim cands

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
            // printfn "UpdateCandidates"

            let ncandidates =
                eliminated
                |> Seq.fold (fun (acc: seq<Cell>) (cell: Cell) ->
                             acc
                             |> Seq.filter (fun (cell2: Cell) ->
                                            not (cell.Pos = cell2.Pos && cell.Value = cell2.Value)))
                             _candidates

            // printfn "Candidates length %d %d" (Seq.length ncandidates) (Seq.length eliminated)
            _candidates <- ncandidates
            _candidates

        member this.UpdateGrid(newSolved: seq<Cell>) : seq<Cell> =
            // printfn "UpdateGrid"

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
                                            not (cell.Pos = cell2.Pos
                                                 || (cell.Pos.Sees(cell2.Pos)
                                                     && cell.Value = cell2.Value))))
                             _candidates

            // printfn "Candidates length %d %d" (Seq.length ncandidates) (Seq.length newSolved)
            _candidates <- ncandidates
            _candidates

        member this.Solve() : bool =
            let cnt = Seq.length _candidates

            printfn "Start from %d candidates" cnt

            let finders : List<seq<Cell> -> FindResult> = [
                findSinglesSimple
                findHiddenSingles
                findNakedSets 2
                findNakedSets 3
                findHiddenSets 2
                findHiddenSets 3
                findNakedSets 4
                findHiddenSets 4
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
