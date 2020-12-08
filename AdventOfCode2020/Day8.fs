module Day8

open System;
open System.Diagnostics;
open System.IO;

type Instruction =
    | Acc of param : int
    | Jmp of param : int
    | Nop of param : int
;;

let parse (s : string) : Instruction =
    match s.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "acc"; n |] -> Acc (Int32.Parse(n));
    | [| "jmp"; n |] -> Jmp (Int32.Parse(n));
    | [| "nop"; n |] -> Nop (Int32.Parse(n));
    | _ -> failwithf "Unrecognised input %s" s;


let execute (a : int) (p : int) (i : Instruction) : int * int =
    match i with
    | Acc x -> (a+x, p+1);
    | Jmp x -> (a, p+x);
    | Nop _ -> (a, p+1);

let rec simulate (a : int) (p : int) (ps : int list) (is : Instruction list) : bool * int =
    if p >= is.Length then
        (true, a);
    elif List.contains p ps then
        (false, a);
    else
        let (a', p') = execute a p is.[p];
        simulate a' p' (p::ps) is;

let rec fix (p : int) (is : Instruction list) =
    if p >= is.Length then
        failwith "Ran out of instructions to change";
    
    match is.[p] with
    | Acc _ ->  fix (p+1) is;
    | Jmp x ->  let (a, b) = is
                                |> List.mapi (fun i j -> if i=p then Nop x else j)
                                |> simulate 0 0 [];
                if a then b else fix (p+1) is;                
    | Nop x ->  let (a, b) = is
                                |> List.mapi (fun i j -> if i=p then Jmp x else j)
                                |> simulate 0 0 [];
                if a then b else fix (p+1) is;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = ["nop +0";
                "acc +1";
                "jmp +4";
                "acc +3";
                "jmp -3";
                "acc -99";
                "acc +1";
                "jmp -4";
                "acc +6" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> simulate 0 0 []
    |> snd
    |> printfn "Day 8, part 1: %d";

    if testMode then test else input
    |> fix 0
    |> printfn "Day 8, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;