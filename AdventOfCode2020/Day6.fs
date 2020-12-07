module Day6

open System;
open System.Diagnostics;
open System.IO;

let rec groupify (qs : string list) (rs : string list) : string list list =
    match rs with
    | [] -> [qs];
    | x::xs when x = "" -> (qs |> List.rev) :: (groupify [] xs);
    | x::xs -> groupify (x::qs) xs;

let rec intersect (xs : 'a list) (ys : 'a list) : 'a list =
    match xs, ys with
    | x::xs', y::ys' ->
        if   x = y then x :: intersect xs' ys'
        elif x < y then intersect xs' ys
        else            intersect xs  ys'
    | _ -> []

let rec commonAnswers (ps : string list) : char list =
    match ps with
    | [] -> failwith "No input!";
    | [x] -> x |> Seq.distinct |> Seq.toList |> List.sort ;
    | x::xs ->  x |> Seq.distinct |> Seq.toList |> List.sort |> intersect (commonAnswers xs);

let rec allAnswers (ps : string list) : char list =
    match ps with
    | [] -> failwith "No input!";
    | [x] -> x |> Seq.toList |> List.distinct;
    | x::xs -> ((x |> Seq.toList) @ allAnswers xs) |> List.distinct;

let score (f : string list -> char list) (gs : string list list) : int =
    gs
    |> List.map (f)
    |> List.map (List.length)
    |> List.fold ((+)) 0;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "abc";
                "";
                "a";
                "b";
                "c";
                "";
                "ab";
                "ac";
                "";
                "a";
                "a";
                "a";
                "a";
                "";
                "b"]
                |> groupify [];

    let input = Seq.toList(File.ReadLines(file))
                |> groupify [];

    if testMode then test else input
    |> score allAnswers
    |> printfn "Day 6, part 1: %d";

    if testMode then test else input
    |> score commonAnswers
    |> printfn "Day 6, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;