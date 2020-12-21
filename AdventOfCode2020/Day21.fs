module Day21

open System;
open System.Diagnostics;
open System.IO;

type Ingredient = {
    Name : string;
};;

type Food = {
    Ingredients : string list;
    Allergens : string list;
};;

let parse (s : string) : Food =
    match s.Split([| '('; ')' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a; b |] -> let al = b.Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.tail;
                    let is = a.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList;
                    { Ingredients = is; Allergens = al };
    | _ -> failwithf "Invalid input %s" s;

let intersect (a : 'a list) (b : 'a list) : 'a list =
    Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList;

let allergyFree (fs : Food list) : string list =
    let is = fs |> List.map (fun f -> f.Ingredients) |> List.concat |> List.distinct;
    let al = fs |> List.map (fun f -> f.Allergens) |> List.concat |> List.distinct;

    let us = al
                |> List.map (fun a -> fs 
                                        |> List.filter (fun f -> f.Allergens |> List.exists (fun b -> b = a))
                                        |> List.map (fun f -> f.Ingredients)
                                        |> (fun x -> List.head x, List.tail x)
                                        ||> List.fold (fun a x -> intersect a x))
                |> List.concat
                |> List.distinct;

    List.except us is;

let occurrences (fs : Food list) (is : string list) : int =
    is |> List.sumBy (fun i -> fs |> List.filter (fun f -> List.contains i f.Ingredients) |> List.length);

let rec allergens (a : (string * string) list) (fs : Food list) =
    let al = fs |> List.map (fun f -> f.Allergens)
                |> List.concat
                |> List.distinct
                |> List.map (fun a -> (a, fs 
                                        |> List.filter (fun f -> f.Allergens |> List.exists (fun b -> b = a))
                                        |> List.map (fun f -> f.Ingredients)
                                        |> (fun x -> List.head x, List.tail x)
                                        ||> List.fold (fun a x -> intersect a x)))
                |> List.filter (fun (_, is) -> List.length is = 1)
                |> List.map (fun (a, is) -> (a, List.head is))

    match al with
    | [] -> a
            |> List.sortBy (fun (a, _) -> a)
            |> List.map snd
            |> (fun is -> List.head is, List.tail is)
            ||> List.fold (fun a i -> a + "," + i);
    | xs -> fs
            |> List.map (fun f -> { Ingredients = List.except (xs |> List.map snd) f.Ingredients; Allergens = List.except (xs |> List.map fst) f.Allergens })
            |> allergens (xs@a);

let canonicalAllergens (fs : Food list) (is : string list) =
    fs
    |> List.map (fun f -> { Ingredients = List.except is f.Ingredients; Allergens = f.Allergens })
    |> allergens []

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)";
                "trh fvjkl sbzzf mxmxvkd (contains dairy)";
                "sqjhc fvjkl (contains soy)";
                "sqjhc mxmxvkd sbzzf (contains fish)" ]
                |> List.map parse;

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    if testMode then test else input
    |> (fun i -> (i, allergyFree i))
    ||> occurrences
    |> printfn "Day 21, part 1: %d";

    if testMode then test else input
    |> (fun i -> (i, allergyFree i))
    ||> canonicalAllergens
    |> printfn "Day 21, part 2: %s";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
