module Day11

open System;
open System.Diagnostics;
open System.IO;

type Space =
    | Empty
    | Floor
    | Occupied
;;

let parse (s : string) : Space list =
    s
    |> Seq.toList
    |> List.map (fun x -> match x with
                            | 'L' -> Empty;
                            | '#' -> Occupied;
                            | '.' -> Floor;
                            | _ -> failwithf "Unrecognised input %c!" x);

let prettyPrintRow (s : Space list) : unit =
    for t in s do
        match t with
        | Empty -> printf "L";
        | Floor -> printf ".";
        | Occupied -> printf "#";

let prettyPrintGrid (ss : Space list list) : unit =
    for s in ss do
        prettyPrintRow s;
        printfn "";
    printfn "";

let createPoints ((lx, ly) : int * int) ((ux, uy) : int * int) ((fx, fy) : int * int) : (int * int) list =
    let lx' = Math.Max(lx, fx-1);
    let ux' = Math.Min(ux, fx+1)
    let ly' = Math.Max(ly, fy-1);
    let uy' = Math.Min(uy, fy+1);

    [ for j in ly'..uy' do
        for i in lx'..ux' -> 
            (i, j)
    ]
    |> List.filter (fun (x, y) -> x <> fx || y <> fy);

let rec findSeat (r : bool) ((fx, fy) : int * int) (ss : Space list list) ((dx, dy) : int * int) : Space option =
    let fx' = fx + dx;
    let fy' = fy + dy;
    if fx' < 0 || fx' >= ss.[0].Length || fy' < 0 || fy' >= ss.Length then
        None;
    else
        match ss.[fy'].[fx'] with
        | Floor ->  if r then
                        findSeat r (fx', fy') ss (dx, dy);
                    else
                        Some Floor;
        | s -> Some s;

let step (l : int) (r : bool) (ss : Space list list) : Space list list =
    [ for j in 1..ss.Length ->
        [ for i in 1..ss.[j-1].Length ->
            let ps = [(-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)]
                    |> List.map (fun d -> findSeat r (i-1, j-1) ss d)
                    |> List.choose id;

            match ss.[j-1].[i-1] with
            | Empty ->  if ps |> List.forall (fun x -> x <> Occupied) then
                            Occupied;
                        else
                            Empty;
            | Floor -> Floor;
            | Occupied ->   if ps |> List.filter (fun x -> x = Occupied) |> List.length >= l then
                                Empty;
                            else
                                Occupied;
        ]
    ];

let stable (ps : Space list list) (qs : Space list list) : bool =
    (qs |> List.concat)
    |> List.zip (ps |> List.concat)
    |> List.forall (fun (x, y) -> x = y);

let rec stabilise (l : int) (r : bool) (ss : Space list list) : int =
    let ts = step l r ss;

    match stable ss ts with
    | true -> ts |> List.concat |> List.filter (fun x -> x = Occupied) |> List.length;
    | false -> stabilise l r ts;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "L.LL.LL.LL";
                "LLLLLLL.LL";
                "L.L.L..L..";
                "LLLL.LL.LL";
                "L.LL.LL.LL";
                "L.LLLLL.LL";
                "..L.L.....";
                "LLLLLLLLLL";
                "L.LLLLLL.L";
                "L.LLLLL.LL" ]
                |> List.map (parse);
                
    let input = Seq.toList(File.ReadLines(file))
                |> List.map (parse);
    
    if testMode then test else input
    |> stabilise 4 false
    |> printfn "Day 11, part 1: %d";

    if testMode then test else input
    |> stabilise 5 true
    |> printfn "Day 11, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;