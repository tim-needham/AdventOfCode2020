module Day23

open System;
open System.Diagnostics;
open System.IO;

let generateLinks (cs : int list) : int array =
    let mutable ls = Array.init (1 + List.length cs) (fun _ -> -1);
    for k, v in List.pairwise cs do 
        ls.[k] <- v;
       
    ls.[cs |> List.rev |> List.head] <- List.head cs;
    ls;

let rec findCup (c : int) (m : int) (ts : int list) : int =
    let d = if c = 1 then m else c - 1;
    match List.contains d ts with
    | false -> d;
    | true -> findCup d m ts;

let rec followLinks (c : int) (ls : byref<int array>) : int list =
    match ls.[c] with
    | 1 -> [];
    | x -> x::(followLinks x &ls);

let rec move (n : int) (c : int) (m : int) (ls : byref<int array>) : bool =
    match n with
    | 0 ->  true;
    | _ ->  // take 3 cups after the current
            let ts = [ ls.[c]; ls.[ls.[c]]; ls.[ls.[ls.[c]]] ];
            // cut them out by linking the current cup to the last cup's link
            ls.[c] <- ls.[ts.[2]];
            // find the destination cup
            let d = findCup c m ts;
            // find the next cup after that
            let l = ls.[d];
            // insert the taken cups after the destination
            ls.[d] <- ts.[0];
            // rejoin the next cup to the taken cups
            ls.[ts.[2]] <- l
            // recurse
            move (n-1) ls.[c] m &ls;

let crabGame (m : int) (cs : int list) : string =
    let mutable ls = generateLinks cs;
    ignore (move m (List.head cs) (List.max cs) &ls);

    followLinks 1 &ls
    |> List.fold (fun a x -> a + x.ToString()) ""

let crabGame2 (m : int) (cs : int list) : int64 =
    let mutable ls = generateLinks cs;
    ignore (move m (List.head cs) (List.max cs) &ls);
    int64 ls.[1] * int64 ls.[ls.[1]];

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ 3; 8; 9; 1; 2; 5; 4; 6; 7 ];
    let input = [ 9; 6; 2; 7; 1; 3; 8; 5; 4 ];

    if testMode then test else input
    |> crabGame 100 
    |> printfn "Day 23, part 1: %s";

    (if testMode then test else input) @ [10..1000000]
    |> crabGame2 10000000
    |> printfn "Day 23, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
