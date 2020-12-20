module Day19

open System;
open System.Diagnostics;
open System.IO;

type RuleType =
    | Literal of c : char
    | Sequence of ids : int list
    | Adjunct of ids : int list * int list
;;

type Rule =
    {
        Id : int;
        Type : RuleType;
};;

let parseType (cs : string list) : RuleType =
    match cs with
    | [x] ->    match x.[0] with
                | '"' -> Literal x.[1];
                | _ -> Sequence [Int32.Parse(x)];
    | xs -> match xs|> List.tryFindIndex (fun x -> x = "|") with
            | None -> Sequence (xs |> List.map Int32.Parse);
            | Some i -> xs
                        |> List.splitAt i
                        |> (fun (a, b) -> (a, b |> List.tail))
                        |> (fun (a, b) -> Adjunct (a |> List.map Int32.Parse, b |> List.map Int32.Parse)); 

let parseRule (s : string) : Rule =
    match s.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> { Id = Int32.Parse(a); Type = b.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> parseType };
    | _ -> failwithf "Unrecognised input %s" s;

let parseInput (s : string) : char list =
    s |> Seq.toList;

let rec parse (b : bool) (ss : string list) : Rule list * char list list =
    match ss with
    | [] -> ([], []);
    | ""::xs -> parse false xs;
    | x::xs ->  let (ps, qs) = parse b xs;
                if b then ((parseRule x)::ps, qs) else (ps, (parseInput x)::qs);

let rec stackRules (rs : int list) (m : Map<int, RuleType>) (cs : char list) : bool =
    match rs, cs with
    | [], [] -> true;
    | _, [] -> false;
    | [], _ -> false;
    | x::xs, c::ys ->   match Map.find x m with
                        | Literal l -> if l = c then stackRules xs m ys else false;
                        | Sequence ss -> stackRules (ss@xs) m cs;
                        | Adjunct (ps, qs) -> if stackRules (ps@xs) m cs then true else stackRules (qs@xs) m cs;

let matchRules (rs : Rule list) (cs : char list) : bool =
    let m = rs
            |> List.map (fun r -> (r.Id, r.Type))
            |> Map.ofList

    stackRules [0] m cs;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "0: 4 1 5";
                "1: 2 3 | 3 2";
                "2: 4 4 | 5 5";
                "3: 4 5 | 5 4";
                "4: \"a\"";
                "5: \"b\"";
                "";
                "ababbb";
                "bababa";
                "abbbab";
                "aaabbb";
                "aaaabbb" ]
                |> parse true;

    let test2 = Seq.toList(File.ReadAllLines("test19.txt"))
                |> parse true;

    let input = Seq.toList(File.ReadLines(file))
                |> parse true;

    let rules, inputs = if testMode then test2 else input

    inputs
    |> List.map (fun x -> matchRules rules x)
    |> List.filter (fun x -> x)
    |> List.length
    |> printfn "Day 19, part 1: %d";

    let rules' = rules
                    |> List.map (fun x ->   match x.Id with
                                            | 8 -> parseRule "8: 42 | 42 8";
                                            | 11 -> parseRule "11: 42 31 | 42 11 31";
                                            | _ -> x);
    inputs
    |> List.map (fun x -> matchRules rules' x)
    |> List.filter (fun x -> x)
    |> List.length
    |> printfn "Day 19, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
