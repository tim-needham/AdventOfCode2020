module Day5

open System;
open System.Diagnostics;
open System.IO;

let rec row (c : float) (bs : char list) : int =
    match bs with
    | [] -> 0;
    | x::xs when x = 'B' ->  int (2.0 ** c) + row (c-1.0) xs;
    | x::xs when x = 'F' ->  row (c-1.0) xs;
    | x::xs -> failwithf "Invalid character found in sequence %c" x;

let rec column (c : float) (bs : char list) : int =
    match bs with
    | [] -> 0;
    | x::xs when x = 'R' ->  int (2.0 ** c) + column (c-1.0) xs;
    | x::xs when x = 'L' ->  column (c-1.0) xs;
    | x::xs -> failwithf "Invalid character found in sequence %c" x;

let seat (bs : char list) : int =
    bs
    |> List.splitAt 7
    |> (fun (x, y) -> (row 6.0 x, column 2.0 y))
    |> (fun (x, y) -> (8 * x) + y);

let filter (ps : int list) (qs : int list) : int =
    let rs = qs |> List.sort;

    ps
    |> List.sort
    |> List.filter (fun x -> not (List.contains x rs) && (List.contains (x-1) rs && (List.contains (x+1) rs)))
    |> List.head;
    

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "BFFFBBFRRR";
                "FFFBBBFRRR";
                "BBFFBBFRLL" ];

    let input = Seq.toList(File.ReadLines(file));

    if testMode then test else input
    |> List.map (Seq.toList)
    |> List.map (seat)
    |> List.max
    |> printfn "Day 5, part 1: %d";

    input
    |> List.map (Seq.toList)
    |> List.map (seat)
    |> filter [0..1027]
    |> printfn "Day 5, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;