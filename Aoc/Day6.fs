module Aoc.Day6

open Parsing

let data = read "Aoc/data/day6.txt"

let solve uniqueLength (data: string) =
    let rec solve' pos =
        if (Seq.distinct data[pos - uniqueLength + 1..pos] |> Seq.length) = uniqueLength then pos
        else solve' (pos + 1)
    solve' (uniqueLength - 1) + 1
    
let part1 = solve 4
let part2 = solve 14