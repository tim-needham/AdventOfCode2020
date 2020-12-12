module Day12

open System;
open System.Diagnostics;
open System.IO;

type Move =
    | N of x : int
    | S of x : int
    | E of x : int
    | W of x : int
    | L of x : int
    | R of x : int
    | F of x : int
;;

let parse (s : string) : Move =
    match s |> Seq.toList with
    | 'N'::xs -> N (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'S'::xs -> S (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'E'::xs -> E (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'W'::xs -> W (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'L'::xs -> L (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'R'::xs -> R (Int32.Parse (xs |> Array.ofList |> String.Concat));
    | 'F'::xs -> F (Int32.Parse (xs |> Array.ofList |> String.Concat));    
    | _ -> failwithf "Unrecognised input %s" s;

let rotate (x : int) (d : int) : int =
    (x + d + 360) % 360;

let forward ((px, py) : int * int) (d : int) (n : int) : int * int =
    match d with
    | 0 -> (px, py - n);
    | 90 -> (px + n, py);
    | 180 -> (px, py + n);
    | 270 -> (px - n, py);
    | _ -> failwith "Looks like my assumption about angles was incorrect!";

let rotateWaypoint((wx, wy) : int * int) (n : int) : int * int =
    // signs will change based on quadrant
    // (-, -) | (+, -)
    // ---------------
    // (-. +) | (+, +)
    match (n + 360) % 360 with
    | 0 -> (wx, wy);
    | 90 -> match wx >= 0, wy >= 0 with
            | true, true -> (-wy, wx);
            | false, true -> (-wy, wx);
            | false, false -> (-wy, wx);
            | true, false -> (-wy, wx);
    | 180 -> (-wx, -wy);
    | 270 ->    match wx >= 0, wy >= 0 with
                | true, true -> (wy, -wx);
                | false, true -> (wy, -wx);
                | false, false -> (wy, -wx);
                | true, false -> (wy, -wx);
    | _ -> failwith "Looks like my assumption about angles was incorrect!";

let forwardToWaypoint ((wx, wy) : int * int) ((sx, sy) : int * int) (n : int) : int * int =
    (sx + (n * wx), sy + (n * wy));

let rec travel ((px, py) : int * int) (d : int) (ms : Move list) : int * int =
    match ms with
    | [] -> (px, py);
    | N (x)::xs -> travel (px, py - x) d xs;
    | S (x)::xs -> travel (px, py + x) d xs;
    | E (x)::xs -> travel (px + x, py) d xs;
    | W (x)::xs -> travel (px - x, py) d xs;
    | L (x)::xs -> travel (px, py) (rotate -x d) xs;
    | R (x)::xs -> travel (px, py) (rotate x d) xs;
    | F (x)::xs -> travel (forward (px, py) d x) d xs;    

let rec moveToWaypoint ((wx, wy) : int * int) ((sx, sy) : int * int) (ms : Move list) : int * int =
    match ms with
    | [] -> (sx, sy);
    | N (x)::xs -> moveToWaypoint (wx, wy - x) (sx, sy) xs;
    | S (x)::xs -> moveToWaypoint (wx, wy + x) (sx, sy) xs;
    | E (x)::xs -> moveToWaypoint (wx + x, wy) (sx, sy) xs;
    | W (x)::xs -> moveToWaypoint (wx - x, wy) (sx, sy) xs;
    | L (x)::xs -> moveToWaypoint (rotateWaypoint (wx, wy) -x) (sx, sy) xs;
    | R (x)::xs -> moveToWaypoint (rotateWaypoint (wx, wy) x) (sx, sy) xs;
    | F (x)::xs -> moveToWaypoint (wx, wy) (forwardToWaypoint (wx, wy) (sx, sy) x) xs;    


let manhattan ((px, py) : int * int) : int =
    Math.Abs(px) + Math.Abs(py);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "F10";
                "N3";
                "F7";
                "R90";
                "F11" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> travel (0, 0) 90
    |> manhattan
    |> printfn "Day 12, part 1: %A";

    if testMode then test else input
    |> moveToWaypoint (10, -1) (0, 0)
    |> manhattan
    |> printfn "Day 12, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;