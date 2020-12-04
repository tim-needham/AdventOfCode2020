module Day1

open System;
open System.Diagnostics;
open System.IO;

let rec pairs (l : 'a list) : seq<('a * 'a)> = 
    seq { 
        match l with
        | h::t -> 
            yield! t |> Seq.map (fun elem -> (h, elem))
            yield! t |> pairs
        | _ -> ()
    };

let rec triples (l : 'a list) : seq<('a * 'a * 'a)> =
    seq {
        match l with
        | h1::h2::t ->
            yield! h2::t |> pairs |> Seq.map (fun (a, b) -> (h1, a, b))
            yield! h2::t |> triples
        | _ -> ()
    };

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    let (a, b) = input
                |> pairs
                |> Seq.find(fun (x, y) -> x + y = 2020)

    (a * b)
    |> printfn "Day 1, part 1: %d";

    let (c, d, e) = input
                    |> triples
                    |> Seq.find(fun (x, y, z) -> x + y + z = 2020)
    (c * d * e)
    |> printfn "Day 1, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;