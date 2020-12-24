module Day24

open System;
open System.Diagnostics;
open System.IO;

let rec walk ((x, y) : int * int) (ss : char list) : int * int =
    match ss with
    | [] -> (x, y);
    | 'e'::xs -> walk (x + 2, y) xs;
    | 'w'::xs -> walk (x - 2, y) xs;
    | 's'::'e'::xs -> walk (x + 1, y + 1) xs;
    | 's'::'w'::xs -> walk (x - 1, y + 1) xs;
    | 'n'::'e'::xs -> walk (x + 1, y - 1) xs;
    | 'n'::'w'::xs -> walk (x - 1, y - 1) xs;

let rec walkAll (ts : (int * int) list) (ss : char list list) : (int * int) list =
    match ss with
    | [] -> ts;
    | x::xs ->  let (p, q) = walk (0, 0) x;
                let ts' =   if List.contains (p, q) ts then
                                List.except [(p, q)] ts
                            else
                                (p, q)::ts;
                walkAll ts' xs;

let gridify (ts : (int * int) list) : (int * int * int * int) * Map<int * int, bool> =
    let (lx, ux, ly, uy) = ts |> List.fold (fun (a, b, c, d) (p, q) -> (Math.Min(a, p), Math.Max(b, p), Math.Min(c, q), Math.Max(d, q))) (0, 0, 0, 0);

    [ for j in ly..uy do
        for i in lx..ux ->
            (i, j);
    ]
    // Hexagonal grid -> even cells on even rows, odd cells on odd rows.
    |> List.filter (fun (p, q) -> Math.Abs(p%2) = Math.Abs(q%2))
    |> List.map (fun x -> (x, List.contains x ts))
    |> Map.ofList
    |> (fun x -> ((lx, ux, ly, uy), x));

let getNeighbours ((x, y) : int * int) (ts : Map<int * int, bool>) : bool list =
    [(-1, -1); (1, -1); (-2, 0); (2, 0); (-1, 1); (1, 1)]
    |> List.map (fun (p, q) -> (x+p, y+q))
    |> List.map (fun t -> Map.tryFind t ts)
    |> List.choose id;

let rec evolve (n : int) ((lx, ux, ly, uy) : int * int * int * int) (ts : Map<int * int, bool>) : Map<int * int, bool> =
    if n = 0 then
        ts;
    else
        let lx', ux', ly', uy' = lx-2, ux+2, ly-1, uy+1;
        [ for j in ly'..uy' do
            for i in lx'..ux' ->
                (i, j);
        ]
        |> List.filter (fun (p, q) -> Math.Abs(p%2) = Math.Abs(q%2))
        |> List.map(fun x ->    let b = match Map.tryFind x ts with
                                        | None -> false;
                                        | Some b -> b
                                let ns = getNeighbours x ts |> List.filter (fun y -> y) |> List.length;
                                if b && (ns = 0 || ns > 2) then
                                    (x, false);
                                elif (not b) && ns = 2 then
                                    (x, true);
                                else
                                    (x, b))
        |> Map.ofList
        |> evolve (n-1) (lx', ux', ly', uy');

let life (n : int) (ts : (int * int) list) : (int * int) list =
    ts
    |> gridify
    ||> evolve n
    |> Map.toList
    |> List.filter (fun (_, b) -> b)
    |> List.map fst;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = Seq.toList(File.ReadAllLines("test24.txt"))
                |> List.map Seq.toList;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map Seq.toList;

    if testMode then test else input
    |> walkAll []
    |> List.length
    |> printfn "Day 24, part 1: %d";

    if testMode then test else input
    |> walkAll []
    |> life 100
    |> List.length
    |> printfn "Day 24, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
