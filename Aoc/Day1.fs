module Aoc.Day1

open Aoc.Parsing
open System

let data =
    read "Aoc/data/day1.txt"
    |> splitByTwoLines
    |> List.map (fun blk -> splitByLine blk |> List.map (fun n -> Int32.Parse(n)))
    
let part1 (data: int list list) = data |> List.map List.sum |> List.max
let part2 (data: int list list) = data |> List.map List.sum |> List.sortDescending |> List.take 3 |> List.sum