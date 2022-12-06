module Aoc.Day5

open System
open System.Collections.Generic
open Parsing

let getMoves (s: string) =
    splitByLine s
    |> List.map(fun mv ->
        let spl = splitBySpace mv
        Int32.Parse spl[1], Int32.Parse spl[3], Int32.Parse spl[5])
    
let getStacks (s: string) =
    let lines = splitByLine s |> Array.ofList
    let stacks = List<Stack<char>>()
    for y in [1..4..lines[0].Length] do
        let s = Stack<char>()
        stacks.Add(s)
        for x in [0..lines.Length - 2] |> List.rev do
            let c = lines[x][y]
            if not (Char.IsWhiteSpace(c)) then s.Push(lines[x][y])
    stacks

let data () =
    read "Aoc/data/day5.txt"
    |> splitByTwoLines
    |> fun a -> getStacks a[0], getMoves a[1]
    
let part1 (data: List<Stack<char>> * (int * int * int) list) =
    let stacks, moves = data
    moves |> List.iter (fun (cnt, dirFrom, dirTo) ->
        for _ in [1..cnt] do
            stacks[dirFrom - 1].Pop() |> stacks[dirTo - 1].Push
    )
    Seq.map (fun (s: Stack<char>) -> s.Peek()) stacks |> Array.ofSeq |> String
    
let part2 (data: List<Stack<char>> * (int * int * int) list) =
    let stacks, moves = data
    moves |> List.iter (fun (cnt, dirFrom, dirTo) ->
        let mutable chars = []
        for _ in [1..cnt] do
            chars <- stacks[dirFrom - 1].Pop()::chars
        for c in chars do
            stacks[dirTo - 1].Push(c)
    )
    Seq.map (fun (s: Stack<char>) -> s.Peek()) stacks |> Array.ofSeq |> String