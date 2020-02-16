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

module Combinations =
    type Combinations(n: int, k: int) =
        let mutable _cjs : int [] = Array.append [| for i in 0..k -> (i - 1) |] [| n; 0 |]
        let mutable _j = k
        let mutable _first = true

        // do
        //     printfn "_cjs %A %d"  _cjs (Array.length _cjs)

        let t6 (cjs: int []) (j: int) (x: int) : bool =
            cjs.[j] <- x
            _j <- j - 1

            true

        let t4 (cjs: int []) : bool =
            let rec loop j x =
                cjs.[j - 1] <- j - 2

                let nx = cjs.[j] + 1

                if nx = cjs.[j + 1] then
                    loop (j + 1) nx
                else
                    (j, nx)

            match loop 2 -1 with
                | (j, x) when j > k ->
                    _j <- j
                    false // T5
                | (j, x) ->
                    t6 cjs j x

        member this.elems () : int [] =
            _cjs.[1 .. k]

        member this.toSeq : seq<int []> =
            Seq.unfold (fun (gen: Combinations) ->
                        if _first then
                          _first <- false
                          Some(gen.elems(), gen)
                        elif gen.next() then
                          Some(gen.elems(), gen)
                        else
                          None)
                        this

        member this.next () : bool =
            match (_j, _cjs.[1], _cjs.[2]) with
                | (j, cjs1, cjs2) when j > 0 ->
                    //  T6
                    _cjs.[j] <- j
                    _j <- j - 1

                    true
                | (_, cjs1, cjs2) when (cjs1 + 1) < cjs2 ->
                    // T3
                    _cjs.[1] <- cjs1 + 1

                    true
                | (j, cjs1, cjs2) -> t4 _cjs
