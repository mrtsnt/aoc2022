module Aoc.Parsing

open System
open System.IO

let splitBy (seps: string[]) (str: string)  = str.Split(seps, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
let splitByTwoLines = splitBy [| "\n\n"; "\r\n\r\n" |]
let splitByLine = splitBy [| "\n"; "\r\n" |]
let splitBySpace = splitBy [| " " |]
let read (str: string) = File.ReadAllText(str)
