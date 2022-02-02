#!/usr/bin/env -S dotnet fsi

#r "nuget: Fake.Core.Process, 5.21"

open System.IO
open Fake.Core
open System

let (+/) (path1: string) path2 = Path.Join(path1, path2)

let selectedNums =
    fsi.CommandLineArgs
    |> Seq.choose (fun s ->
        match Int32.TryParse(s) with
        | (true, i) -> Some i
        | (false, _) -> None)

let nums =
    if Seq.isEmpty selectedNums then
        [ 1 .. 25 ]
    else
        selectedNums
        |> Seq.filter (fun i -> i > 0 && i <= 25)
        |> Seq.sort
        |> Seq.toList

for i in nums do
    let dir = sprintf "day%02d" i
    let file = sprintf "day%d.fsx" i
    let path = dir +/ file

    printfn "Day %d:" i

    if File.Exists(path) then
        CreateProcess.fromRawCommand "dotnet" [ "fsi"; path; dir +/ "input.txt" ]
        |> Proc.run
        |> ignore
    else
        printfn "Nothing to run for day %d" i

    printfn ""
