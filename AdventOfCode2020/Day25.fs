module Day25

open System;
open System.Diagnostics;
open System.IO;

let loop (s : int64) (i : int64) : int64 =
    (i * s) % 20201227L;

let rec determineLoopSize (n : int) (s : int64) (i : int64) (t : int64) : int =
    if i = t then
        n;
    else
        determineLoopSize (n+1) s (loop s i) t;

let rec encrypt (p : int64) (s : int64) (l : int) : int64 =
    if l = 0 then
        p;
    else
        encrypt (loop s p) s (l-1);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ 5764801L; 17807724L ];

    let input = [ 11562782L; 18108497L ];

    let cKey = (if testMode then test else input) |> List.head;
    let dKey = (if testMode then test else input) |> List.rev |> List.head;

    cKey
    |> determineLoopSize 0 7L 1L
    |> encrypt 1L dKey
    |> printfn "Day 25, part 1: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;