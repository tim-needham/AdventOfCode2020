module Day3

open System;
open System.Diagnostics;
open System.IO;

let rec traverse (cs : char list) (p : int) (w : int) (h : int) (m : string list) : char list =
    match m with
    | x::xs ->  if h = 1 then
                    traverse (x.[p]::cs) ((p+w) % x.Length) w h xs;
                else                    
                    let ys = if xs = [] then [] else xs |> List.skip (h-1)
                    traverse (x.[p]::cs) ((p+w) % x.Length) w h ys;
    | _ -> cs |> List.rev |> List.tail;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "..##.......";
                "#...#...#..";
                ".#....#..#.";
                "..#.#...#.#";
                ".#...##..#.";
                "..#.##.....";
                ".#.#.#....#";
                ".#........#";
                "#.##...#...";
                "#...##....#";
                ".#..#...#.#" ];

    let input = Seq.toList(File.ReadLines(file));

    if testMode then test else input
    |> traverse [] 0 3 1
    |> List.filter(fun x -> x = '#')
    |> List.length
    |> printfn "Day 3, part 1: %d";

    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map (fun (x, y) -> traverse [] 0 x y (if testMode then test else input))
    |> List.map (List.filter (fun x -> x = '#'))
    |> List.map (List.length)
    |> List.fold (fun a x -> a * int64 x) 1L
    |> printfn "Day 3, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;