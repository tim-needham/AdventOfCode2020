module Day17

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : bool list =
    s |> Seq.toList |> List.map (fun x -> match x with '#' -> true | _ -> false);

let hyperSpace (g : bool list list) : bool list list list list =
    [[ g ]];

let generateNeighbours ((x, y, z, w) : int * int * int * int) ((mx, my, mz, mw) : int * int * int * int) : (int * int * int * int) list =
    [ for l in Math.Max(0, w-1)..Math.Min(mw, w+1) do
        for k in Math.Max(0, z-1)..Math.Min(mz, z+1) do    
            for j in Math.Max(0, y-1)..Math.Min(my, y+1) do
                for i in Math.Max(0, x-1)..Math.Min(mx, x+1) ->
                    (i, j, k, l)
    ]
    |> List.filter (fun (i, j, k, l) -> not (i=x && j=y && k=z && l=w));
    
// Ok, let's grow the space each time instead of operating in a fixed-size space.
// h : bool - Hypercube!
let cycle (h : bool) (g : bool list list list list) : bool list list list list =
    let mx, my, mz, md = g.[0].[0].[0].Length-1, g.[0].[0].Length-1, g.[0].Length-1, g.Length-1;

    [ for l in (if h then -1 else 0)..(md + if h then 1 else 0) ->
        [ for k in -1..mz+1 ->    
            [ for j in -1..my+1->
                [ for i in -1..mx+1 ->
                    let ns = generateNeighbours (i, j, k, l) (mx, my, mz, md)
                            |> List.map (fun (x, y, z, w) -> g.[w].[z].[y].[x])
                            |> List.filter (fun x -> x)
                            |> List.length;
                    let c = if l >= 0 && l <= md && k >= 0 && k <= mz && j >= 0 && j <= my && i >= 0 && i <= mx then g.[l].[k].[j].[i] else false;
                    if c && (ns = 2 || ns = 3) then true;
                    elif (not c) && ns = 3 then true;
                    else false;
                ]
            ]
        ]
    ];

let rec cycles (h : bool) (n : int) (g : bool list list list list) : bool list list list list =
    match n with
    | 0 -> g;
    | x -> g |> cycle h |> cycles h (x-1);

let energy (g : bool list list list list) : int =
    g |> List.map List.concat |> List.map List.concat |> List.concat |> List.filter (fun x -> x) |> List.length;

let prettyPrint (g : bool list list list list) : unit =
    for l in 0..g.Length-1 do
        for k in 0..g.[l].Length-1 do
            for j in 0..g.[l].[k].Length-1 do
                for i in 0..g.[l].[k].[j].Length-1 do
                    if g.[l].[k].[j].[i] then printf "#" else printf ".";
                printfn "";
            printfn "";
        printfn "";
        
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ ".#.";
                "..#";
                "###" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> hyperSpace
    |> cycles false 6
    |> energy
    |> printfn "Day 17, part 1: %d";

    if testMode then test else input
    |> hyperSpace
    |> cycles true 6
    |> energy
    |> printfn "Day 17, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
