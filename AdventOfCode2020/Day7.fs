module Day7

open System;
open System.Diagnostics;
open System.IO;

let rec bagify (ss : string list) : (int * string) list =
    match ss with
    | [] -> [];
    | t::ts ->  match t.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
                | [| x; y; z; _ |] -> (Int32.Parse(x), y + " " + z) :: bagify ts;
                | [| "no"; "other"; "bags." |] -> [];
                | _ -> failwithf "Unrecognised input %s" t;

let findBags (s : string) : (int * string) list =
    match s.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| |] -> [];
    | xs -> xs
            |> Array.toList
            |> bagify;

let parse (s : string) : (string * (int * string) list) =
    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
    | x::y::"bags"::"contain"::xs -> (x + " " + y, xs |> List.fold (fun a c -> a + " " + c) "" |> findBags );
    | _ -> failwithf "Unrecognised input %s" s;

let rec expand (m : Map<string, (int * string) list>) (b : string) : string list =
    match Map.find b m with
    | [] -> [b];
    | xs -> b :: (xs |> List.map (fun (_, y) -> expand m y) |> List.concat);

let expansions (bs : (string * (int * string) list) list) : string list list =
    let m = bs |> Map.ofList;

    bs
    |> List.map (fst)
    |> List.map (fun b -> expand m b);

let combinations (b : string) (bs : (string * (int * string) list) list) : string list =
    bs
    |> expansions
    |> List.filter (List.contains b)
    |> List.map (List.head)
    |> List.filter (fun x -> x <> b)
    |> List.distinct;

let rec countBags (m : Map<string, (int * string) list>) (b : string) : int =
    match Map.find b m with
    | [] -> 0;
    | xs -> xs
            |> List.sumBy (fun (x, y) -> x * (1 + countBags m y));

let contents (b : string) (bs : (string * (int * string) list) list) : int =
    countBags (Map.ofList bs) b;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = ["light red bags contain 1 bright white bag, 2 muted yellow bags.";
                "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
                "bright white bags contain 1 shiny gold bag.";
                "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
                "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
                "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
                "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
                "faded blue bags contain no other bags.";
                "dotted black bags contain no other bags." ]
                |> List.map parse;

    let test2 = ["shiny gold bags contain 2 dark red bags.";
                "dark red bags contain 2 dark orange bags.";
                "dark orange bags contain 2 dark yellow bags.";
                "dark yellow bags contain 2 dark green bags.";
                "dark green bags contain 2 dark blue bags.";
                "dark blue bags contain 2 dark violet bags.";
                "dark violet bags contain no other bags." ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> combinations "shiny gold"
    |> List.length
    |> printfn "Day 7, part 1: %d";

    if testMode then test2 else input
    |> contents "shiny gold"
    |> printfn "Day 7, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;