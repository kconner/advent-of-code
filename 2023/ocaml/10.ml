#!/usr/bin/env ocaml

(* Model *)

type position = int * int
type heading = L | U | R | D
type viewpoint = position * heading
type square = char
type dimensions = (int * int)
type map = (position, square) Hashtbl.t
type turn = LT | RT
type step = turn option * viewpoint
type path = step Seq.t
type path_side = LH | RH
type position_set = (position, unit) Hashtbl.t

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

let start_square_and_heading (map : map) ((sx, sy) : position) : (square * heading) =
    let connections = List.filter
        (fun ((dx, dy), adjacent_square_filter, heading, _) ->
            let adjacent = (sx + dx, sy + dy) in
            match Hashtbl.find_opt map adjacent with
            | None -> false
            | Some square -> String.contains adjacent_square_filter square)
        [((-1, 0), "-LF", L, "-J7"); ((0, -1), "|7F", U, "|LJ"); ((1, 0), "-J7", R, "-LF"); ((0, 1), "|LJ", D, "|7F")] in
    let ((_, _, heading, squaresA), (_, _, _, squaresB)) = match connections with
        | a :: b :: [] -> (a, b)
        | _ -> failwith "< 2 connections" in
    let common_square =
        Seq.find
            (fun square -> String.contains squaresB square)
            (String.to_seq squaresA)
        |> Option.get in
    (common_square, heading)

let map_dimensions_and_start (lines : string list) : (map * dimensions * viewpoint) =
    let map = Hashtbl.create 400000 in
    let dimensions = (List.hd lines |> String.length, List.length lines) in
    let start_position = ref (-1, -1) in
    List.iteri
        (fun y line ->
            String.iteri
                (fun x square ->
                    Hashtbl.add map (x, y) square ;
                    if square = 'S' then start_position := (x, y))
                line)
        lines ;
    let (start_square, start_heading) = start_square_and_heading map !start_position in
    Hashtbl.replace map !start_position start_square;
    (map, dimensions, (!start_position, start_heading))

let (map, dimensions, start_viewpoint) = with_file "10.txt" file_lines |> map_dimensions_and_start

(* Problem 1 *)

let next_position ((position, heading) : viewpoint) : position =
    match heading with
    | L -> (fst position - 1, snd position)
    | U -> (fst position, snd position - 1)
    | R -> (fst position + 1, snd position)
    | D -> (fst position, snd position + 1)

let turn_and_next_heading (square : square) (heading : heading) : (turn option * heading) =
    match (square, heading) with
    | ('-', L) -> (None, L) | ('-', R) -> (None, R)
    | ('|', U) -> (None, U) | ('|', D) -> (None, D)
    | ('L', D) -> (Some LT, R) | ('L', L) -> (Some RT, U)
    | ('J', D) -> (Some RT, L) | ('J', R) -> (Some LT, U)
    | ('7', U) -> (Some LT, L) | ('7', R) -> (Some RT, D)
    | ('F', U) -> (Some RT, R) | ('F', L) -> (Some LT, D)
    | _ -> failwith "Invalid square"

let next_step (map : map) (viewpoint : viewpoint) : step =
    let next_position = next_position viewpoint in
    let next_square = Hashtbl.find map next_position in
    let (turn, next_heading) = turn_and_next_heading next_square (snd viewpoint) in
    (turn, (next_position, next_heading))

let path_from_start (map : map) (viewpoint : viewpoint) : step Seq.t =
    let end_position = fst viewpoint in
    Seq.unfold
        (fun viewpoint ->
            match viewpoint with
            | None -> None
            | Some viewpoint ->
            let next_step = next_step map viewpoint in
            let next_viewpoint = snd next_step in
            if fst next_viewpoint = end_position
                then Some (next_step, None)
                else Some (next_step, Some next_viewpoint))
        (Some viewpoint)

let problem1 () =
    start_viewpoint
    |> path_from_start map
    |> Seq.length
    |> fun count -> count / 2
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let path_inside (path : path) : path_side =
    let right_turns_minus_left = path
        |> Seq.filter_map fst
        |> Seq.map (function RT -> 1 | LT -> -1)
        |> Seq.fold_left (+) 0 in
    if right_turns_minus_left = 4 then RH
    else if right_turns_minus_left = -4 then LH
    else failwith "Invalid path"

let positions_on_path (path : path) : position_set =
    let positions_on_path = Hashtbl.create 400000 in
    path |> Seq.iter (fun (_, (position, _)) -> Hashtbl.add positions_on_path position ()) ;
    positions_on_path

let walkable_inside_headings (inside : path_side) ((turn, (_, heading)) : step) : heading list =
    match (inside, turn, heading) with
    | (RH, Some RT, _) -> [] | (LH, Some LT, _) -> []
    | (RH, None, L) -> [U] | (RH, None, U) -> [R] | (RH, None, R) -> [D] | (RH, None, D) -> [L]
    | (LH, None, L) -> [D] | (LH, None, D) -> [R] | (LH, None, R) -> [U] | (LH, None, U) -> [L]
    | (RH, Some LT, L) -> [R; U] | (RH, Some LT, U) -> [D; R] | (RH, Some LT, R) -> [L; D] | (RH, Some LT, D) -> [U; L]
    | (LH, Some RT, L) -> [R; D] | (LH, Some RT, D) -> [U; R] | (LH, Some RT, R) -> [L; U] | (LH, Some RT, U) -> [D; L]

let positions_inside_path (path : path) (inside : path_side) (positions_on_path : position_set) : position_set =
    let positions_inside_path = Hashtbl.create 400000 in
    path
    |> Seq.iter (fun step -> 
        walkable_inside_headings inside step
        |> List.iter (fun heading ->
            let rec walk ((px, py) : position) ((dx, dy) : (int * int)) : unit =
                let position = (px + dx, py + dy) in
                if Hashtbl.mem positions_on_path position
                    then ()
                    else (
                        Hashtbl.replace positions_inside_path position () ;
                        walk position (dx, dy)
                    )
            in
            let (_, (position, _)) = step in
            let velocity = match heading with
                | L -> (-1, 0) | U -> (0, -1) | R -> (1, 0) | D -> (0, 1) in
            walk position velocity)
        ) ;
    positions_inside_path

let problem2 () =
    let path = path_from_start map start_viewpoint in
    let inside = path_inside path in
    let positions_on_path = positions_on_path path in
    positions_inside_path path inside positions_on_path
    |> Hashtbl.length
    |> string_of_int
    |> print_endline

let () = problem1 () ; problem2 ()
