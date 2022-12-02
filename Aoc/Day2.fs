module Aoc.Day2

open Aoc.Parsing

type Shape = R | P | S
type Outcome = Win | Lose | Draw

let getShape = function
    | "A" | "X" -> R
    | "B" | "Y" -> P
    | "C" | "Z" -> S
    | _ -> failwith "unknown"
    
let getOutcome = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "unknown"
        
let getShapes = function
    | [ x; y ] -> getShape x, getShape y
    | _ -> failwith "unknown"
    
let getShapeOutcome = function
    | [ x; y ] -> getShape x, getOutcome y
    | _ -> failwith "unknown"
    
let data =
    read "Aoc/data/day2.txt"
    |> splitByLine
    |> List.map (fun ln -> splitBySpace ln |> getShapes)
    
let data2 = 
    read "Aoc/data/day2.txt"
    |> splitByLine
    |> List.map (fun ln -> splitBySpace ln |> getShapeOutcome)
    
let shapeScore = function R -> 1 | P -> 2 | S -> 3
    
let resultScore x y =
    match x, y with
    | a, b when a = b -> 3
    | R, P | S, R | P, S -> 6
    | _ -> 0
    
let neededShape o r =
    match (o, r) with
    | _, Draw -> o
    | S, Win -> R
    | R, Win -> P
    | P, Win -> S
    | S, Lose -> P
    | R, Lose -> S
    | P, Lose -> R
    
let outcomeScore = function Win -> 6 | Draw -> 3 | Lose -> 0
    
let roundScore (a, b) = shapeScore b + resultScore a b
let roundScore2 (a, b) = outcomeScore b + (neededShape a b |> shapeScore)
let part1 data = List.map roundScore data |> List.sum
let part2 data = List.map roundScore2 data |> List.sum