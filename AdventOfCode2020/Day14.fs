module Day14

open System;
open System.Diagnostics;
open System.IO;

type Instruction =
    | Assign of a : int64 * v : int64
    | Mask of m : char list
;;

let parse (s : string) : Instruction =
    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "mask"; "="; n |] -> n |> Seq.toList |> Mask;
    | [| ad; "="; vl |] -> Assign (ad.Substring(4, ad.Length - 5) |> Int64.Parse, Int64.Parse(vl));
    | _ -> failwithf "Unrecognised input %s" s;
    
let rec b2l (bs : bool list) : float * int64 =
    match bs with
    | [] -> (0.0, 0L);
    | x::xs ->  b2l xs
                |> (fun (p, q) -> (p + 1.0, (if x then int64 (2.0 ** p) else 0L) + q));

let bitsToLong (bs : bool list) : int64 =
    b2l bs |> snd;

let rec l2b (e : int) (l : int64) : bool list =
    match e with
    | 0 -> [(l = 1L)];
    | _ ->  let p = int64 (2.0 ** (float) e);
            let l' = if l >= p then l - p else l;
            (l >= p) :: (l2b (e-1) l')

let longToBits (l : int64) : bool list =
    l2b 35 l;

let rec applyMask (ms : char list) (bs : bool list) : bool list =
    match ms, bs with
    | [], [] -> [];
    | [], _ | _, [] -> failwith "Unequal input lengths!";
    | x::xs, y::ys -> (if x = '1' then true elif x = '0' then false else y) :: (applyMask xs ys);

let rec applyMask2 (ms : char list) (bs : bool list) : seq<bool list> =
    seq {
        match ms, bs with
        | [], [] -> ([]);
        | [], _ | _, [] -> failwith "Unequal input lengths!";
        | '0'::xs, y::ys -> yield! (applyMask2 xs ys) |> Seq.map (fun m -> y::m);  
        | '1'::xs, y::ys -> yield! (applyMask2 xs ys) |> Seq.map (fun m -> true::m);  
        | 'X'::xs, y::ys -> yield! (applyMask2 xs ys) |> Seq.map (fun m -> false::m);
                            yield! (applyMask2 xs ys) |> Seq.map (fun m -> true::m);
        | _, _ -> failwith "Invalid input!";
    };

let rec part1 (mk : char list) (is : Instruction list) (ms : Map<int64, int64>) : Map<int64, int64> =
    match is with
    | [] -> ms;
    | Mask m::xs -> part1 m xs ms;
    | Assign (x, y)::xs ->  y
                            |> longToBits
                            |> applyMask mk
                            |> bitsToLong
                            |> (fun m -> (m, ms))
                            ||> Map.add x 
                            |> part1 mk xs;

let rec part2 (mk : char list) (is : Instruction list) (ms : Map<int64, int64>) : Map<int64, int64> =
    match is with
    | [] -> ms;
    | Mask m::xs -> part2 m xs ms;
    | Assign (x, y)::xs ->  x
                            |> longToBits
                            |> applyMask2 mk
                            |> Seq.map bitsToLong
                            |> Seq.fold (fun m k -> Map.add k y m) ms
                            |> part2 mk xs;

let processInstructions (f : (char list) -> (Instruction list) -> (Map<int64, int64>) -> Map<int64, int64>) (is : Instruction list) : int64 =
    f [] is Map.empty<int64, int64>
    |> Map.toList
    |> List.fold (fun a (_, v) -> a + v) 0L

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X";
                "mem[8] = 11";
                "mem[7] = 101";
                "mem[8] = 0" ]
                |> List.map parse;

    let test2 = [ "mask = 000000000000000000000000000000X1001X";
                "mem[42] = 100";
                "mask = 00000000000000000000000000000000X0XX";
                "mem[26] = 1" ]
                |> List.map parse;
     
    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> processInstructions part1
    |> printfn "Day 14, part 1: %d";

    if testMode then test2 else input
    |> processInstructions part2
    |> printfn "Day 14, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;