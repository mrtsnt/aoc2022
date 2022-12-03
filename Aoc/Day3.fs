module Aoc.Day3

open Aoc.Parsing

let data = read "Aoc/data/day3.txt" |> splitByLine

let getCommon ss = Set.intersectMany (List.map Set.ofSeq ss) |> Seq.head
let splitByHalf data = List.map (fun (s: string) -> [s[0..s.Length / 2 - 1]; s[s.Length / 2..]]) data
let splitByThree data = List.chunkBySize 3 data

let solve splitter data  =
    let priorities = ['a'..'z'] @ ['A'..'Z'] |> List.mapi (fun i x -> (x, i + 1)) |> Map.ofSeq
    splitter data |> List.map getCommon |> List.sumBy (fun c -> Map.find c priorities)
    
let part1 data = solve splitByHalf data
let part2 data = solve splitByThree data