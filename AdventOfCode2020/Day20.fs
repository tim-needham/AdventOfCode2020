module Day20

open System;
open System.Diagnostics;
open System.IO;

type Image = {
    Id : int;
    Data : bool list list;
};;

type Edge =
    | Top
    | Bottom
    | Left
    | Right;;

let parseData (s : string) : bool list =
    s
    |> Seq.toList
    |> List.map (fun x -> x = '#')

let rec parse (i : int) (ds : bool list list) (ss : string list) : Image list =
    match ss with
    | [] -> [{ Id = i; Data = ds |> List.rev }];
    | x::xs ->  match x with
                | "" -> { Id = i; Data = ds |> List.rev } :: parse 0 [] xs;
                | y when y.StartsWith("Tile") -> parse (Int32.Parse(y.Substring(5, y.Length-6))) [] xs;
                | _ -> parse i ((parseData x)::ds) xs;

let prettyPrint (i : Image) : unit =
    printfn "Tile %d:" i.Id;
    for y in i.Data do
        for x in y do
            printf "%c" (if x then '#' else '.')
        printfn "";
    printfn "";

let getEdge (e : Edge) (i : Image) : bool list =
    match e with
    | Top -> i.Data.[0];
    | Bottom -> i.Data.[i.Data.Length-1];
    | Left -> [ for j in i.Data -> j.[0] ];
    | Right -> [ for j in i.Data -> j.[j.Length-1] ];

let matches (a : bool list) (b : bool list) : bool =
    a |> List.zip b |> List.forall (fun (x, y) -> x = y);

let rotate (im : Image) : Image =
    { 
        Id = im.Id;
        Data = [ for j in 0..im.Data.Length-1 ->
                    [ for i in 0..im.Data.[j].Length-1 ->
                        im.Data.[im.Data.Length-i-1].[j]
                    ]
                ];
    };

let flipH (im : Image) : Image =
    {
        Id = im.Id;
        Data = [ for j in 0..im.Data.Length-1 ->
                    [ for i in 0..im.Data.[j].Length-1 ->
                        im.Data.[j].[im.Data.[j].Length-i-1]
                    ]
                ];
    };
    
let flipV (im : Image) : Image =
    {
        Id = im.Id;
        Data = [ for j in 0..im.Data.Length-1 ->
                    [ for i in 0..im.Data.[j].Length-1 ->
                        im.Data.[im.Data.Length-j-1].[i]
                    ]
                ];
    };

let rotations (i : Image) : Image list =
    [0..2]
    |> List.fold (fun a _ -> (a |> List.head |> rotate)::a) [i]
    |> List.rev;
    
let orientations (i : Image) : Image list =
    i
    |> rotations
    |> List.map (fun x -> [x; flipH x; flipV x])
    |> List.concat
    |> List.distinct;

let candidate (a : Image) (b : Image) (e : Edge) : bool =
    match e with
    | Top -> matches (getEdge Bottom a) (getEdge Top b);
    | Left -> matches (getEdge Right a) (getEdge Left b);
    | _ -> failwithf "Edge doesn't fit progression %A" e;

let candidates (l : Image option) (t : Image option) (c : Image) : Image list =
    c
    |> orientations
    |> List.filter (fun x ->    match l with
                                | Some a -> candidate a x Left;
                                | None -> true)
    |> List.filter (fun x ->    match t with
                                | Some a -> candidate a x Top;
                                | None -> true);

let rec place (d : int) (ps : Image option list list) ((x, y) : int * int) (is : int list) (m : Map<int, Image>) =
    // did we finish?
    if ps |> List.concat |> List.filter (fun x -> x = None) |> List.length = 0 then
        ps |> List.map (fun a -> a |> List.choose id) |> Some;
    else
        // image ids already used
        let used = ps |> List.map (fun a -> a |> List.choose id) |> List.concat |> List.map (fun a -> a.Id);
        // image ids remaining
        let ids = is |> List.except used;
        // check above
        let t = if y > 0 then ps.[y-1].[x] else None;
        // check left
        let l = if x > 0 then ps.[y].[x-1] else None;
        // candidate images
        let cs = ids
                    |> List.map (fun c -> Map.find c m)
                    |> List.map (fun c -> candidates l t c)
                    |> List.concat;

        // try them all, if any
        match cs with
        | [] -> None;
        | zs -> let x' = if x < d - 1 then x + 1 else 0;
                let y' = if x < d - 1 then y else y + 1;

                match zs |> List.map (fun z ->  let ps' = [ for j in 0..ps.Length-1 -> [ for i in 0..ps.[j].Length-1 -> if i = x && j = y then Some z else ps.[j].[i] ] ];
                                                place d ps' (x', y') is m)
                        |> List.choose id with
                | [] -> None;
                | os -> os |> List.head |> Some;

let solve (is : Image list) : Image list list =
    let d = int32 (Math.Sqrt(float (is |> List.length)));
    let m = is |> List.map (fun i -> i.Id, i) |> Map.ofList;
    let ps = [ for j in 1..d -> [ for i in 1..d -> None ] ];
    
    place d ps (0, 0) (is |> List.map (fun i -> i.Id)) m
    |> (fun x -> x.Value);

let corners (ps : Image list list) : int64 = 
    ps
    |> (fun x -> [x.[0]; x.[x.Length-1] ])
    |> List.map (fun x -> [x.[0].Id; x.[x.Length - 1].Id ])
    |> List.concat
    |> List.fold (fun a x -> a * (int64 x)) 1L;

let rec slice (ss : string list list) : string list list =
    match ss |> List.head with
    | [] -> [];
    | _ ->  (ss |> List.map List.head) :: slice (ss |> List.map List.tail);

let trimBorder (im : Image) : bool list list =
    [ for j in 1..(im.Data.Length-2) ->
        [ for i in 1..(im.Data.[j].Length-2) ->
            im.Data.[j].[i]
        ]
    ];

// requires all images to be the same size
let collectDataRow (is : Image list) : string list =
    is
    |> List.map trimBorder
    |> List.map (fun i -> i |> List.map (fun j -> j |> List.fold (fun a x -> a + if x then "#" else ".") ""))
    |> slice
    |> List.map (fun i -> i |> List.fold (fun a x -> a + x) "")

let collectData (is : Image list list) : string list =
    is
    |> List.map collectDataRow
    |> List.concat;

let rec findMonster ((x, y) : int * int) (ms : bool list list) (ss : bool list list) =
    if y >= ss.Length - ms.Length - 1 then
        [];
    else
        let x' = if x < ss.[0].Length - ms.[0].Length - 1 then x + 1 else 0;
        let y' = if x < ss.[0].Length - ms.[0].Length - 1 then y else y + 1;

        if  [ for j in 0..ms.Length-1 ->
                [ for i in 0..ms.[j].Length-1 ->
                    if ms.[j].[i] then ss.[j+y].[i+x] else true
                ]
            ]
            |> List.concat
            |> List.forall (fun a -> a) then
            (x, y) :: findMonster (x', y') ms ss;
        else
            findMonster (x', y') ms ss;

let findMonsters (ms : bool list list) (ss : Image) =
    ss
    |> orientations
    |> List.map (fun x -> findMonster (0, 0) ms x.Data)
    |> List.filter (fun x -> x <> [])
    |> List.head;

let roughness (ms : bool list list) (ss : Image) : int =
    let ps = findMonsters ms ss;
    let s = ss.Data
            |> List.concat
            |> List.filter (fun x -> x)
            |> List.length;
    let m = ms
            |> List.concat
            |> List.filter (fun x -> x)
            |> List.length;

    s - (m * List.length ps);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = Seq.toList(File.ReadAllLines("test20.txt"))
                |> parse 0 [];

    let input = Seq.toList(File.ReadLines(file))
                |> parse 0 [];

    let monster = [ "                  # ";
                    "#    ##    ##    ###";
                    " #  #  #  #  #  #   " ]
                    |> List.map (fun x -> x |> Seq.toList |> List.map (fun y -> y = '#'));

    let image = if testMode then test else input
                |> solve

    image
    |> collectData
    |> (fun x -> File.WriteAllLines("./image20.txt", x));

    image
    |> corners
    |> printfn "Day 20, part 1: %d";

    let seas = if testMode then "testImage20.txt" else "image20.txt"
                |> (fun x -> File.ReadAllLines(x))
                |> Seq.toList
                |> List.map parseData
                |> (fun x -> { Id = 1; Data = x })

    seas
    |> roughness monster
    |> printfn "Day 20, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
