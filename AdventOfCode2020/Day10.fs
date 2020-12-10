module Day10

open System;
open System.Diagnostics;
open System.IO;

let rec assemble (os : int list) (d : int) (ts : int list) : int list option =
    match ts with
    | [] -> (d::os) |> List.rev |> Some;
    | x::xs ->  if x <= (os |> List.head) + 3 then
                    assemble (x::os) d xs;
                else
                    None;

let scan (ts : int list) : int =
    let d = (ts |> List.max) + 3;

    match assemble [0] d ts with
    | None -> 0;
    | Some os ->    let ps, qs = os
                                |> List.pairwise
                                |> List.map (fun (x, y) -> y - x)
                                |> List.partition (fun x -> x = 1);
                    (ps |> List.length) * (qs |> List.length);

let rec candidates (t : int) (ts : int list) : (int * int list) list =
    match ts with
    | [] -> [];
    | x::xs ->  if x > t + 3 then
                    []
                else
                    (x, xs)::(candidates t xs);

let rec combine (os : int list) (ts : int list) : int list list =
    match ts with
    | [] -> [os];
    | _ ->  let cs = candidates (os |> List.head) ts;
            cs
            |> List.map (fun (x, y) -> combine (x::os) y)
            |> List.concat;

// Chunk the sequence into subsequences when there's a gap of three between consecutive numbers
// run the solution on each subsequence then multiply.
let rec chunkBy3 (ss : int list) (ts : int list) : int list list =
    match ts with
    | [] -> [ss |> List.rev];
    | x::xs ->  match ss with
                | [] -> chunkBy3 [x] xs
                | s::_ ->   if x < s + 3 then
                                chunkBy3 (x::ss) xs;
                            else
                                match xs with
                                | [] -> ([x]::[ ss |> List.rev ]) |> List.rev;
                                | _ -> (ss |> List.rev)::(chunkBy3 [x] xs);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4 ];

    let test2 = [28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2 ;34; 10; 3 ];

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    if testMode then test2 else input
    |> List.sort
    |> scan
    |> printfn "Day 10, part 1: %d";

    if testMode then test2 else input
    |> List.sort
    |> (fun x -> 0::x)
    |> chunkBy3 []
    |> List.map (fun x -> combine [List.head x] (List.tail x))
    |> List.map (List.length)
    |> List.fold (fun a x -> a * int64 x) 1L
    |> printfn "Day 10, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;