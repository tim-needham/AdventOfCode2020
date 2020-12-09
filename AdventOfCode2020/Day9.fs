module Day9

open System;
open System.Diagnostics;
open System.IO;

let rec pairs (l : 'a list) : seq<('a * 'a)> = 
    seq { 
        match l with
        | h::t -> 
            yield! t |> Seq.map (fun elem -> (h, elem))
            yield! t |> pairs
        | _ -> ()
    };
    
let valid (ws : int64 list) (n : int64) : bool =
    ws
    |> pairs
    |> Seq.map (fun (x, y) -> x + y)
    |> Seq.filter (fun x -> x = n)
    |> Seq.length
    |> (fun x -> x > 0);

let rec scan (ws : int64 list) (ns : int64 list) : int64 option =
    match ns with
    | [] -> None;
    | x::xs ->  if valid ws x then
                    scan ((ws |> List.skip 1)@[x]) xs;
                else
                    Some x;

let firstInvalid (w : int) (ns : int64 list) : int64 =
    match scan (ns |> List.take w) (ns |> List.skip w) with
    | Some n -> n;
    | None -> failwith "All inputs are valid!";

let rec something (t : int64) (a : int64 list) (ns : int64 list) : int64 list =
    match ns with
    | [] -> [];
    | x::xs ->  let a' = x + (List.sum a);
                if a' > t then
                    [];
                elif a' = t then
                    a @ [x];
                else
                    something t (a @ [x]) xs;

let rec crack (t : int64) (ns : int64 list) : int64 =
    match ns with
    | [] -> failwith "Unable to find contiguous sequence!";
    | _::xs ->  match something t [] ns with
                | [] -> crack t xs;
                | ys -> ( ys |> List.min) + (ys |> List.max);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ 35L;
                20L;
                15L;
                25L;
                47L;
                40L;
                62L;
                55L;
                65L;
                95L;
                102L;
                117L;
                150L;
                182L;
                127L;
                219L;
                299L;
                277L;
                309L;
                576L ];

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int64.Parse(x.ToString()));

    let window = if testMode then 5 else 25;

    let invalid =   if testMode then test else input
                    |> firstInvalid window

    invalid
    |> printfn "Day 9, part 1: %d";

    if testMode then test else input
    |> crack invalid
    |> printfn "Day 9, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;