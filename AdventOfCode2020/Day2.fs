module Day2

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : (int * int * char * string) =
    match s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b; c; d |] -> (Int32.Parse(a), Int32.Parse(b), c.[0], d);

let rec verify (a : int) (n : int) (x : int) (c : char) (d : char list) : bool =
    match d with
    | s::ss when s = c -> verify (a+1) n x c ss;
    | s::ss -> verify a n x c ss;
    | _ -> (a >= n) && (a <= x);

let verify2 (n : int) (x : int) (c : char) (d : char list) : bool =
    (d.[n-1] = c) <> (d.[x-1] = c);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ (1, 3, 'a', "abcde"); (1, 3, 'b', "cdefg"); (2, 9, 'c', "ccccccccc")]; 

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> parse(x.ToString()));

    if testMode then test else input
    |> List.filter(fun (a, b, c, d) -> d |> Seq.toList |> verify 0 a b c)
    |> List.length
    |> printfn "Day 2, part 1: %d";

    if testMode then test else input
    |> List.filter(fun (a, b, c, d) -> d |> Seq.toList |> verify2 a b c)
    |> List.length
    |> printfn "Day 2, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;