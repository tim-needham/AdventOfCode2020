module Day24

open System;
open System.Diagnostics;
open System.IO;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    0
    |> printfn "Day 24, part 1: %d";

    0
    |> printfn "Day 24, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
