#!/usr/bin/env ocaml -I +str

#load "str.cma"

(* Model *)

type node_id = string
type edges = node_id * node_id
type step = edges -> node_id

let left_step : step = fun (l, _) -> l
let right_step : step = fun (_, r) -> r

type route = step Seq.t

let make_route (s : string) : route =
    String.to_seq s
    |> Seq.map (function
        | 'L' -> left_step
        | 'R' -> right_step
        | _ -> failwith "invalid turn")
    |> Seq.cycle

type network = (node_id, edges) Hashtbl.t

(* Parsing *)

let with_file (filename : string) (f : in_channel -> 'a) : 'a =
    let ic = open_in filename in
    let result = f ic in
    close_in ic ; result

let file_lines (channel : in_channel) : string list =
    let rec loop (lines : string list) : string list =
        try
            let line = input_line channel in
            loop (line :: lines)
        with
        | End_of_file -> List.rev lines
    in
    loop []

let node_regex = Str.regexp "\\([A-Z]+\\) = (\\([A-Z]+\\), \\([A-Z]+\\))"

let parse_node (line : string) : node_id * edges =
    let _ = Str.string_match node_regex line 0 in
    let node_id = Str.matched_group 1 line in
    let left_node_id = Str.matched_group 2 line in
    let right_node_id = Str.matched_group 3 line in
    (node_id, (left_node_id, right_node_id))

let make_network (items : (node_id * edges) list) : network =
    let network = Hashtbl.create 1111 in
    List.iter (fun (node_id, edges) -> Hashtbl.add network node_id edges) items ;
    network

let lines = with_file "8.txt" file_lines
let route = lines |> List.hd |> make_route
let network = lines |> List.tl |> List.tl |> List.map parse_node |> make_network

(* Problem 1 *)

let take_step = (fun (node_id, route, count) ->
    let (step, next_route) = Seq.uncons route |> Option.get in
    let edges = Hashtbl.find network node_id in
    let next_node_id = step edges in
    (next_node_id, next_route, count + 1))

let problem1 () =
    Seq.iterate take_step ("AAA", route, 0)
    |> Seq.find (fun (node_id, _, _) -> node_id = "ZZZ")
    |> Option.get
    |> fun (_, _, count) -> count
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let lcm (denominators : int list) : int =
    let rec gcd (a : int) (b : int) : int =
        if b = 0 then a else gcd b (a mod b)
    in
    let rec lcm (a : int) (b : int) : int =
        a * b / gcd a b
    in
    List.fold_left lcm 1 denominators

let problem2 () =
    Hashtbl.fold (fun k _ acc -> if String.ends_with ~suffix:"A" k then k :: acc else acc) network []
    |> List.map (fun start -> Seq.iterate take_step (start, route, 0)
        |> Seq.find (fun (node_id, _, _) -> String.ends_with ~suffix:"Z" node_id)
        |> Option.get
        |> fun (_, _, count) -> count)
    |> lcm
    |> string_of_int
    |> print_endline

let () = problem1 () ; problem2 ()
