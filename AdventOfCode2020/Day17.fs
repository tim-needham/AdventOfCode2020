module Day17

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : bool list =
    s |> Seq.toList |> List.map (fun x -> match x with '#' -> true | _ -> false);

let padForCycles (hc : bool) (c : int) (g : bool list list) : bool list list list list =
    let h, w = g.Length, g.[0].Length;

    [ for l in 0..(if hc then 2*c else 0) ->
        [ for k in 0..(2*c) ->    
            [ for j in 0..((2*c)+h-1) ->
                [ for i in 0..((2*c)+w-1) ->
                    if k = c && (not hc || l = c) then
                        if j >= c && j < (c+h) && i >= c && i < (c+w) then
                            g.[j-c].[i-c];
                        else
                            false
                    else
                        false
                ]
            ]
        ]
    ];

let generateNeighbours ((x, y, z, w) : int * int * int * int) ((mx, my, mz, mw) : int * int * int * int) : (int * int * int * int) list =
    [ for l in Math.Max(0, w-1)..Math.Min(mw, w+1) do
        for k in Math.Max(0, z-1)..Math.Min(mz, z+1) do    
            for j in Math.Max(0, y-1)..Math.Min(my, y+1) do
                for i in Math.Max(0, x-1)..Math.Min(mx, x+1) ->
                    (i, j, k, l)
    ]
    |> List.filter (fun (i, j, k, l) -> not (i=x && j=y && k=z && l=w));
    

let cycle (g : bool list list list list) : bool list list list list =
    [ for l in 0..g.Length-1 ->
        [ for k in 0..g.[l].Length-1 ->    
            [ for j in 0..g.[l].[k].Length-1 ->
                [ for i in 0..g.[l].[k].[j].Length-1 ->
                    let ns = generateNeighbours (i, j, k, l) (g.[l].[k].[j].Length-1, g.[l].[k].Length-1, g.[l].Length-1, g.Length-1)
                            |> List.map (fun (x, y, z, w) -> g.[w].[z].[y].[x])
                            |> List.filter (fun x -> x)
                            |> List.length;
                    if g.[l].[k].[j].[i] && (ns = 2 || ns = 3) then true
                    elif (not g.[l].[k].[j].[i]) && ns = 3 then true
                    else false;
                ]
            ]
        ]
    ];
    
let rec cycles (n : int) (g : bool list list list list) : bool list list list list =
    match n with
    | 0 -> g;
    | x -> g |> cycle |> cycles (x-1);

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
    |> padForCycles false 6
    |> cycles 6
    |> energy
    |> printfn "Day 17, part 1: %d";

    if testMode then test else input
    |> padForCycles true 6
    |> cycles 6
    |> energy
    |> printfn "Day 17, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
