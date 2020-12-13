module Day13

open System;
open System.Diagnostics;
open System.IO;

let parseRoutes (s : string) : int option list =
    s.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (fun x -> if x = "x" then None else Some (Int32.Parse x));

let parseInput (ss : string list) : int * int option list =
    ss |> List.head |> Int32.Parse, ss |> List.tail |> List.map parseRoutes |> List.concat;

let rec earliestRoute (t : int) (s : int) (b : int) : int =
    if t + b >= s then
        t + b;
    else
        earliestRoute (t + b) s b;

let earliestBus (s : int) (bs : int list) : int * int =
    bs
    |> List.map (fun b -> (b, earliestRoute 0 s b))
    |> List.minBy (fun (_, t) -> t)

let part1 (s : int) (bs : int option list) : int =
    bs
    |> List.choose id
    |> earliestBus s
    |> (fun (b, t) -> (t-s) * b);

let offsets (bs : int option list) : (int64 * int64) list =
    bs
    |> List.mapi (fun i x -> (i, x))
    |> List.choose (fun (i, x) ->   match x with
                                    | None -> None;
                                    | Some y -> Some (int64 i, int64 y));

let rec sieve (t : int64) (s : int64) (bs : (int64 * int64) list) =
    if bs |> List.forall (fun (i, x) -> (t + i) % x = 0L) then
        t;
    else
        let (p, i) = List.head bs;
        if (t + p) % i = 0L then
            let s' = s * i;
            sieve (t + s') s' (List.tail bs);
        else
            sieve (t + s) s bs;

// By inspection, bus identities are pairwise coprime so the Chinese Remainder Theorem applies.
// Find the solution by sieving. Take advantage of the fact that the first bus has offset zero.
let part2 (bs : (int64 * int64) list) =
    bs
    |> (fun x -> List.head x, List.tail x)
    |> (fun ((_, b), t) -> (b, b, t))
    |||> sieve;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "939";
                "7,13,x,x,59,x,31,19" ]
                |> parseInput;

    let input = Seq.toList(File.ReadLines(file))
                |> parseInput;

    if testMode then test else input
    ||> part1
    |> printfn "Day 13, part 1: %d";

    if testMode then test else input
    |> snd
    |> offsets
    |> part2
    |> printfn "Day 13, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;