module Day18

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : char list =
    s |> Seq.toList |> List.filter (fun x -> x <> ' ');

let rec closeParenthesis (l : int) (ps : char list) (qs : char list) : char list * char list =
    match l, qs with
    | 0, _ -> ps |> List.tail |> List.rev, qs;
    | _, [] -> failwith "Unable to find terminating parenthesis";
    | _, '('::xs -> closeParenthesis (l+1) ('('::ps) xs;
    | _, ')'::xs -> closeParenthesis (l-1) (')'::ps) xs;
    | _, x::xs -> closeParenthesis l (x::ps) xs;

let rec evaluate (s : int64) (ts : char list) : int64 =
    match ts with
    | [] -> s;
    | '+'::'('::xs ->   let ps, qs = closeParenthesis 1 [] xs;
                        evaluate (s + evaluate 0L ps) qs;
    | '+'::y::xs -> evaluate (s + Int64.Parse(y.ToString())) xs; 
    | '*'::'('::xs ->   let ps, qs = closeParenthesis 1 [] xs;
                        evaluate (s * evaluate 0L ps) qs;
    | '*'::y::xs -> evaluate (s * Int64.Parse(y.ToString())) xs;
    | '('::xs ->    let ps, qs = closeParenthesis 1 [] xs;
                    evaluate (evaluate 0L ps) qs;
    | x::xs ->  evaluate (Int64.Parse(x.ToString())) xs;

let rec evaluate2 (s : int64) (ts : char list) : int64 =
    match ts with
    | [] -> s;
    | '+'::'('::xs ->   let ps, qs = closeParenthesis 1 [] xs;
                        evaluate2 (s + evaluate2 0L ps) qs;
    | '+'::y::xs -> evaluate2 (s + Int64.Parse(y.ToString())) xs; 
    | '*'::'('::xs ->   let ps, qs = closeParenthesis 1 [] xs;
                        s * (evaluate2 (evaluate2 0L ps) qs);
    | '*'::xs -> s * (evaluate2 0L xs);
    | '('::xs ->    let ps, qs = closeParenthesis 1 [] xs;
                    evaluate2 (evaluate2 0L ps) qs;
    | x::xs ->  evaluate2 (Int64.Parse(x.ToString())) xs;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "1 + 2 * 3 + 4 * 5 + 6" ;
                "1 + (2 * 3) + (4 * (5 + 6))";
                "2 * 3 + (4 * 5)";
                "5 + (8 * 3 + 9 + 3 * 4 * 3)";
                "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))";
                "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
                ] |> List.map parse

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> List.map (fun x -> evaluate 0L x)
    |> List.sum
    |> printfn "Day 18, part 1: %d";

    if testMode then test else input
    |> List.map (fun x -> evaluate2 0L x)
    |> List.sum
    |> printfn "Day 18, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;