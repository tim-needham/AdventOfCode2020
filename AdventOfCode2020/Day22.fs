module Day22

open System;
open System.Diagnostics;
open System.IO;

let rec parse (b : bool) (p1 : int list) (p2 : int list) (ss : string list) : int list * int list =
    match ss with
    | [] -> List.rev p1, List.rev p2;
    | x::xs ->  match x with
                | "Player 1:" -> parse true p1 p2 xs;
                | "Player 2:" -> parse false p1 p2 xs;
                | "" -> parse b p1 p2 xs;
                | _ ->  if b then
                            parse b (Int32.Parse(x)::p1) p2 xs;
                        else 
                            parse b p1 (Int32.Parse(x)::p2) xs;

let score (p : int list) : int =
    p |> List.rev |> List.mapi (fun i x -> (i+1)*x) |> List.sum;

let rec crabCombat (p1 : int list) (p2 : int list) : bool * int =
    match p1, p2 with
    | [], _ -> (false, score p2);
    | _, [] -> (true, score p1);
    | x::xs, y::ys ->   if x > y then
                            crabCombat (xs@[x; y]) ys;
                        else
                            crabCombat xs (ys@[y; x]);

let rec recursiveCombat (ps : (int list * int list) list) (p1 : int list) (p2 : int list) : bool * int =
    if List.exists (fun x -> x = (p1, p2)) ps then
        (true, score p1);
    else
        let ps' = (p1, p2)::ps;

        match p1, p2 with
        | [], _ -> (false, score p2);
        | _, [] -> (true, score p1);
        | x::xs, y::ys ->   if List.length xs >= x && List.length ys >= y then
                                if recursiveCombat [] (List.take x xs) (List.take y ys) |> fst then
                                    recursiveCombat ps' (xs@[x; y]) ys;
                                else
                                    recursiveCombat ps' xs (ys@[y; x]);
                            elif x > y then
                                recursiveCombat ps' (xs@[x; y]) ys;
                            else
                                recursiveCombat ps' xs (ys@[y; x]);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "Player 1:";
                "9";
                "2";
                "6";
                "3";
                "1";
                "";
                "Player 2:";
                "5";
                "8";
                "4";
                "7";
                "10" ]
                |> parse true [] [];

    let input = Seq.toList(File.ReadLines(file))
                |> parse true [] [];

    if testMode then test else input
    ||> crabCombat
    |> snd
    |> printfn "Day 22, part 1: %d";

    if testMode then test else input
    ||> recursiveCombat []
    |> snd
    |> printfn "Day 22, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
