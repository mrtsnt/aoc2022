module Aoc.Day7

open Parsing
open System

type FsObject(name: string, size: int64) =
    let mutable children: FsObject list = []
    member _.Name = name
    member _.Size = size
    member _.Insert(path: string list, object: FsObject) =
        match path with
        | [x] when x = name -> children <- object::children
        | x::xs when x = name -> List.iter (fun (c: FsObject) -> c.Insert(xs, object)) children
        | x::_ when x <> name -> ()
        | _ -> failwith $"failed to insert @ %A{path}"
    member _.GetTotalSize() =
        if size = 0 then List.sumBy (fun (s: FsObject) -> s.GetTotalSize()) children
        else size
    member this.GetChildDirs() =
        if size = 0 then this::(List.collect (fun (c: FsObject) -> c.GetChildDirs()) children)
        else []
        

let data =
    read "data/day7.txt"
    |> splitByLine
    |> Array.ofList
    |> fun lines ->
        let root = FsObject("/", 0)
        let mutable path = ["/"]
        let mutable pos = 1
        while pos < lines.Length do
            match lines[pos].Split(" ") with
            | [| "$"; "ls" |] ->
                pos <- pos + 1
                while pos < lines.Length && lines[pos].Split(" ")[0] <> "$" do
                    match lines[pos].Split(" ") with
                    | [| "dir"; name |] -> root.Insert(List.rev path, FsObject(name, 0))
                    | [| size; name |] -> root.Insert(List.rev path, FsObject(name, Int64.Parse(size)))
                    | _ -> failwith $"unknown subcommand after ls %A{lines[pos]}"
                    pos <- pos + 1
            | [| "$"; "cd"; ".." |] ->
                path <- List.tail path
                pos <- pos + 1
            | [| "$"; "cd"; subdir |] ->
                path <- subdir::path
                pos <- pos + 1
            | _ -> failwith $"unknown subcommand %A{lines[pos]}"
        root
       
let part1 (root: FsObject) =
    root.GetChildDirs()
    |> List.filter (fun (o: FsObject) -> o.GetTotalSize() <= 100_000L)
    |> List.sumBy (fun (o: FsObject) -> o.GetTotalSize())

let part2 (root: FsObject) = 
    let toClean = 30_000_000L - 70_000_000L + data.GetTotalSize()
    root.GetChildDirs()
    |> List.map (fun (o: FsObject) -> o.GetTotalSize())
    |> List.sort
    |> List.find (fun s -> s > toClean)