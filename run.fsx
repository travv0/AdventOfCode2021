#!/usr/bin/env -S dotnet fsi

#r "nuget: Fake.Core.Process, 5.21"

open Fake.Core
open System
open System.IO

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

let verboseCreateProc command args =
    let proc =
        CreateProcess.fromRawCommand command args

    printfn "%s" proc.CommandLine
    proc

let run proc = Proc.run proc |> ignore

let verboseRunProc command = verboseCreateProc command >> run

match nums with
| _ when Seq.length nums = 25 -> "all days"
| [ num ] -> sprintf "day %d" num
| _ ->
    "days "
    + String.Join(", ", Seq.map (sprintf "%d") nums)
|> printfn "\nRunning %s\n"

for i in nums do
    printfn "Day %d:" i

    let dir = sprintf "day%02d" i
    let file = sprintf "day%d" i
    let path = Path.Join(dir, file)
    let fsharpPath = path + ".fsx"

    if File.Exists(fsharpPath) then
        verboseRunProc
            "dotnet"
            [ "fsi"
              fsharpPath
              Path.Join(dir, "input.txt") ]
    elif File.Exists(path + ".ml") then
        CreateProcess.fromRawCommand "esy" []
        |> CreateProcess.redirectOutput
        |> CreateProcess.withWorkingDirectory dir
        |> run

        verboseCreateProc "esy" [ "run" ]
        |> CreateProcess.withWorkingDirectory dir
        |> run
    elif File.Exists(path + ".cabal") then
        verboseCreateProc "cabal" [ "run"; "-v0"; file ]
        |> CreateProcess.withWorkingDirectory dir
        |> run
    else
        printfn "Nothing to run for day %d" i

    printfn ""
