module Aoc.Day8

open System
open System.IO

let data =
    let lines = File.ReadAllText("Aoc/data/day8.txt").Split("\n")
    Array2D.init lines.Length lines.Length (fun x y -> Int32.Parse(string (lines[y][x])))

let part1 (forest: int [,]) =
    let isVisible x y =
        let rangeTo x = if x = 0 then [] else [0..x - 1]
        let rangeFrom x = if x = forest.GetUpperBound(0) then [] else [x + 1..forest.GetUpperBound(0)]
        rangeTo x |> List.forall (fun x' -> forest[x, y] > forest[x', y])
            || rangeFrom x |> List.forall (fun x' -> forest[x, y] > forest[x', y])
            || rangeTo y |> List.forall (fun y' -> forest[x, y] > forest[x, y'])
            || rangeFrom y |> List.forall (fun y' -> forest[x, y] > forest[x, y'])
    
    let mutable visibleSum = 0
    Array2D.iteri (fun x y _ -> if isVisible x y then visibleSum <- visibleSum + 1) forest
    visibleSum
    
    
let part2 (forest: int[,]) =
    let movers =
        [ fun (x, y) -> x, y - 1
          fun (x, y) -> x - 1, y
          fun (x, y) -> x + 1, y
          fun (x, y) -> x, y + 1 ]
        
    let score x y mover =
        let height = forest[x, y]
        let max = forest.GetUpperBound(0)
        let rec score' (x, y) count =
            let x', y' = mover (x, y)
            if x' < 0 || y' < 0 || x' > max || y' > max then count
            elif forest[x', y'] >= height then count + 1
            else score' (x', y') (count + 1)
        score' (x, y) 0
    
    let mutable max = 0
    Array2D.iteri(fun x y _ ->
        let score = movers |> List.map (score x y) |> List.reduce (*)
        if score > max then max <- score
    ) forest
    max