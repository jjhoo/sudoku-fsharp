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

module Permutations =
    type Permutations(n: int) =
        let mutable _ajs : int [] = Array.append [|0|] [| for i in 0..n -> i |]
        let mutable _first = true

        let swap (xs: int []) (i: int) (j: int) : unit =
            let x_i = xs.[i]
            let x_j = xs.[j]

            xs.[i] <- x_j
            xs.[j] <- x_i

            ()

        let l4 (j: int) (l: int) : bool =
            let rec loop k l =
                if k < l then
                    let ign = swap _ajs l k

                    loop (k + 1) (l - 1)
                else
                    (k, l)

            let (k, l) = loop (j + 1) n
            true

        let l3 (j: int) : bool =
            let rec loop l =
                let nl = l - 1

                if _ajs.[j] < _ajs.[nl] then
                    nl
                else
                    loop nl

            let l =
                if _ajs.[j] >= _ajs.[n] then
                    loop n
                else
                    n

            let ign = swap _ajs j l
            l4 j l

        // do
        //     printfn "_ajs %A %d"  _ajs (Array.length _ajs)

        member this.n = n

        member this.elems () : int [] =
            _ajs.[1 .. this.n]

        member this.toSeq : seq<int []> =
            Seq.unfold (fun (gen: Permutations) ->
                        if _first then
                          _first <- false
                          Some(gen.elems(), gen)
                        elif gen.next() then
                          Some(gen.elems(), gen)
                        else
                          None)
                        this

        member this.next () : bool =
             // L2
            let rec loop (j: int) =
                if _ajs.[j] >= _ajs.[j + 1] then
                    loop (j - 1)
                elif _ajs.[j] < _ajs.[j + 1] then
                    j
                elif j = 0 then
                    j
                else
                    loop j

            match loop (this.n - 1) with
                | j when j = 0 -> false
                | j -> l3 j

