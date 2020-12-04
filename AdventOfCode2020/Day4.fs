module Day4

open System;
open System.Diagnostics;
open System.IO;

type Credential = {
    byr : string option;
    iyr : string option;
    eyr : string option;
    hgt : string option;
    hcl : string option;
    ecl : string option;
    pid : string option;
    cid : string option;
};;

let credentialify (m : Map<string, string>) : Credential =
    {
        byr = m |> Map.tryFind "byr";
        iyr = m |> Map.tryFind "iyr";
        eyr = m |> Map.tryFind "eyr";
        hgt = m |> Map.tryFind "hgt";
        hcl = m |> Map.tryFind "hcl";
        ecl = m |> Map.tryFind "ecl";
        pid = m |> Map.tryFind "pid";
        cid = m |> Map.tryFind "cid";
    };

let rec parse (m : Map<string, string>) (s : string list) : seq<Credential> =
    seq{
        match s with
        | x::xs when x <> "" -> let m' =    x.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                                            |> Array.fold (fun a c ->   let cs = c.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries);
                                                                        Map.add cs.[0] cs.[1] a;) m
                                yield! parse m' xs;
        | x::xs ->  yield credentialify m;
                    yield! parse Map.empty xs;
        | _ -> yield credentialify m;
    };

let quickValidate (c : Credential) : bool =
    c.byr <> None && c.iyr <> None && c.eyr <> None && c.hgt <> None && c.hcl <> None && c.ecl <> None && c.pid <> None
    
let validateDate (n : int) (x : int) (d : string option) : bool =
    match d with
    | Some e -> let f = Int32.Parse e;
                f >= n && f <= x;
    | None -> false;

let validateHeight (h : string option) : bool =
    match h with
    | Some x -> match x |> Seq.toList |> List.rev with
                | 'm'::'c'::y ->    let z = y |> List.rev |> String.Concat |> Int32.Parse;
                                    z >= 150 && z <= 193;
                | 'n'::'i'::y ->    let z = y |> List.rev |> String.Concat |> Int32.Parse;
                                    z >= 59 && z <= 76;
                | _ -> false;
    | None -> false;

let validateHair (h : string option) : bool =
    match h with
    | Some x -> match x |> Seq.toList with
                | y::ys when y = '#' && ys.Length = 6 -> ys |> List.forall(fun z -> List.exists ((=) z) ['a'..'f'] || List.exists ((=) z) ['0'..'9']);
                | _ -> false;
    | None -> false;

let validateEyes (e : string option) : bool =
    match e with
    | Some x -> match x with
                | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true;
                | _ -> false;
    | None -> false;

let validatePassport (p : string option) : bool =
    match p with
    | Some x -> let ys = x |> Seq.toList;
                if ys.Length <> 9 then
                    false;
                else
                    ys |> List.forall (fun y -> List.exists ((=) y) ['0'..'9']);
    | None -> false;

let fullValidate (c : Credential) : bool =
    let byr = validateDate 1920 2002 c.byr;
    let iyr = validateDate 2010 2020 c.iyr;
    let eyr = validateDate 2020 2030 c.eyr;
    let hgt = validateHeight c.hgt;
    let hcl = validateHair c.hcl;
    let ecl = validateEyes c.ecl;
    let pid = validatePassport c.pid;

    byr && iyr && eyr && hgt && hcl && ecl && pid;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd";
                "byr:1937 iyr:2017 cid:147 hgt:183cm";
                "";
                "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884";
                "hcl:#cfa07d byr:1929";
                "";
                "hcl:#ae17e1 iyr:2013";
                "eyr:2024";
                "ecl:brn pid:760753108 byr:1931";
                "hgt:179cm";
                "";
                "hcl:#cfa07d eyr:2025 pid:166559648";
                "iyr:2011 ecl:brn hgt:59in" ]
                |> parse Map.empty
                |> Seq.toList;

    let testInvalid = [ "eyr:1972 cid:100";
                        "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926";
                        "";
                        "iyr:2019";
                        "hcl:#602927 eyr:1967 hgt:170cm";
                        "ecl:grn pid:012533040 byr:1946";
                        "";
                        "hcl:dab227 iyr:2012";
                        "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277";
                        "";
                        "hgt:59cm ecl:zzz";
                        "eyr:2038 hcl:74454a iyr:2023";
                        "pid:3556412378 byr:2007" ]
                        |> parse Map.empty
                        |> Seq.toList;

    let testValid   = [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980";
                        "hcl:#623a2f";
                        "";
                        "eyr:2029 ecl:blu cid:129 byr:1989";
                        "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm";
                        "";
                        "hcl:#888785";
                        "hgt:164cm byr:2001 iyr:2015 cid:88";
                        "pid:545766238 ecl:hzl";
                        "eyr:2022";
                        "";
                        "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" ]
                        |> parse Map.empty
                        |> Seq.toList;

    let input = Seq.toList(File.ReadLines(file))
                |> parse Map.empty
                |> Seq.toList;

    if testMode then test else input
    |> List.filter (quickValidate)
    |> List.length
    |> printfn "Day 4, part 1: %d";

    if testMode then (testInvalid@testValid) else input
    |> List.filter (fullValidate)
    |> List.length
    |> printfn "Day 4, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;