module Day16

open System;
open System.Diagnostics;
open System.IO;

type ParseMode =
    | NearbyTickets
    | Rule
    | YourTicket
;;

type Range =
    {
        Min : int;
        Max : int;
    };;

type Rule =
    {
        Label : string;
        Ranges : Range list;
    };;

let parseRange (s : string) : Range =
    s.Split ([| '-' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (Int32.Parse)
    |> (fun x -> { Min = x.[0]; Max = x.[1] });

let parseRule (s : string) : Rule =
    match s.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> b.Split ([| " or " |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (parseRange)
                    |> Array.toList
                    |> (fun x -> { Label = a; Ranges = x });
    | _ -> failwithf "Invalid input %s!" s;

let parseTicket (s : string) : int list =
    s.Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.map (fun x -> Int32.Parse(x.ToString()));

let rec parse (m : ParseMode) (rs : Rule list) (t : int list) (os : int list list) (ss : string list) : Rule list * int list * int list list=
    match ss with
    | [] -> (List.rev rs, t, List.rev os);
    | "your ticket:"::xs -> parse YourTicket rs t os xs;
    | "nearby tickets:"::xs -> parse NearbyTickets rs t os xs;
    | ""::xs -> parse m rs t os xs;
    | x::xs ->  match m with
                | NearbyTickets -> parse m rs t ((parseTicket x)::os) xs;
                | Rule -> parse m ((parseRule x)::rs) t os xs;
                | YourTicket -> parse m rs (parseTicket x) os xs;
                
let findInvalidValues (rs : Rule list) (t : int list) : int list =
    t
    |> List.filter (fun v -> rs |> List.forall (fun r -> r.Ranges |> List.forall (fun g -> v < g.Min || v > g.Max)));

let findInvalidTickets (rs : Rule list) (t : int list) (os : int list list) : int list =
    os
    |> List.map (fun o -> findInvalidValues rs o)
    |> List.concat;

let rec slice (ss : int list list) : int list list =
    match ss |> List.head with
    | [] -> [];
    | _ ->  (ss |> List.map List.head) :: slice (ss |> List.map List.tail);

let findRules (rs : Rule list) (ts : int list) : Rule list =
    rs
    |> List.filter (fun r -> ts |> List.forall (fun t -> r.Ranges |> List.exists (fun g -> g.Min <= t && g.Max >= t)));

let rec simplify (ls : string list) (cs : string list list) : string list =
    if ls.Length = cs.Length then
        cs |> List.map List.head;
    else
        let l = cs |> List.filter (fun c -> c.Length = 1 && (ls |> List.forall (fun x -> x <> c.[0]))) |> List.head |> List.head;
        simplify (l::ls) (cs |> List.map (fun c -> if c = [l] then c else List.except [l] c));

let part2 (k : string) (rs : Rule list) (t : int list) (os : int list list) =
    os
    |> List.filter (fun o -> (findInvalidValues rs o) = [])
    |> slice
    |> List.map (fun at -> findRules rs at |> List.map (fun r -> r.Label))
    |> simplify []
    |> List.zip t
    |> List.filter (fun (_, y) -> y.StartsWith(k))
    |> List.map (fun (i, _) -> i)
    |> List.fold (fun a x -> a * int64 x) 1L;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "class: 1-3 or 5-7";
                "row: 6-11 or 33-44";
                "seat: 13-40 or 45-50";
                "";
                "your ticket:";
                "7,1,14";
                "";
                "nearby tickets:";
                "7,3,47";
                "40,4,50";
                "55,2,20";
                "38,6,12" ]
                |> parse Rule [] [] [];

    let test2 = [ "class: 0-1 or 4-19";
                "row: 0-5 or 8-19";
                "seat: 0-13 or 16-19";
                "";
                "your ticket:";
                "11,12,13";
                "";
                "nearby tickets:";
                "3,9,18";
                "15,1,5";
                "5,14,9" ]
                |> parse Rule [] [] [];

    let input = Seq.toList(File.ReadLines(file))
                |> parse Rule [] [] [];

    if testMode then test else input
    |||> findInvalidTickets
    |> List.sum
    |> printfn "Day 16, part 1: %d";

    if testMode then test2 else input
    |||> part2 "departure"
    |> printfn "Day 16, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
