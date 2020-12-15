module Day15

open System;
open System.Diagnostics;
open System.IO;

let initialMap (s : int list) : Map<int, int list> =
    s
    |> List.mapi (fun i y -> (y, (i+1)))
    |> List.groupBy (fun (y, _) -> y)
    |> List.map (fun (y, is) -> (y, is |> List.map (fun (_, i) -> i)))
    |> List.map (fun (y, is) -> (y, is |> List.sortDescending ))
    |> List.map (fun (y, is) -> (y, if is |> List.length > 2 then is |> List.take 2 else is))
    |> Map.ofList;

let updateMap (m : Map<int, int list>) (n : int) (v : int) : Map<int, int list> =
    match Map.tryFind v m with
    | None -> Map.add v [n] m;
    | Some zs -> Map.add v (n::[List.head zs]) m;

let rec recursiveGenerate (n : int) (s : int list) =
    match n with
    | x when x <= s.Length -> (s.[n-1], initialMap s)
    | x ->  let p, m = recursiveGenerate (n-1) s;
            match Map.find p m with
            | [] -> failwith "Impossible";
            | [_] -> (0, updateMap m x 0);
            | y::ys ->  let v = y - (List.head ys);
                        (v, updateMap m x v);

let saferGenerate (n : int) (s : int list) =
    let m = initialMap s;
    let v = s |> List.rev |> List.head;

    [(s.Length+1)..n]
    |> List.fold (fun (p, m') x ->  match Map.find p m' with
                                    | [] -> failwith "Impossible";
                                    | [_] -> (0, updateMap m' x 0);
                                    | y::ys ->  let v' = y - (List.head ys);
                                                (v', updateMap m' x v')) (v, m)

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ 0; 3; 6 ];

    let input = Seq.toList(File.ReadLines(file))
                |> List.head
                |> (fun x -> x.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries))
                |> Array.toList
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    if testMode then test else input
    |> saferGenerate 2020
    |> fst
    |> printfn "Day 15, part 1: %d";

    if testMode then test else input
    |> saferGenerate 30000000
    |> fst
    |> printfn "Day 15, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;