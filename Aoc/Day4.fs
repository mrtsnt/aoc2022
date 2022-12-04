module Aoc.Day4

open Aoc.Parsing
open System

let data =
    let toSet (range: string) =
        let split = range.Split("-")
        [Int32.Parse(split[0])..Int32.Parse(split[1])] |> Set.ofList
    read "Aoc/data/day4.txt"
    |> splitByLine
    |> List.map (fun l ->
        let split = l.Split(",")
        toSet(split[0]), toSet(split[1]) )
    
let part1 (data: (int Set * int Set) list) =
    List.filter (fun (s1, s2) -> Set.isSubset s1 s2 || Set.isSubset s2 s1) data |> List.length
    
let part2 (data: (int Set * int Set) list) =
    List.filter (fun (s1, s2) -> Set.intersect s1 s2 |> Set.isEmpty |> not) data |> List.length