#!/usr/bin/env ocaml

(* Model *)

type position = int * int
type heading = L | U | R | D
type viewpoint = position * heading
type square = char
type map = (position, square) Hashtbl.t

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
    let common_square = Seq.find (fun square -> String.contains squaresB square) (String.to_seq squaresA) |> Option.get in
    (common_square, heading)

let map_and_start (lines : string list) : (map * viewpoint) =
    let map = Hashtbl.create 400000 in
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
    (map, (!start_position, start_heading))

let (map, start_viewpoint) = with_file "10.txt" file_lines |> map_and_start

(* Debugging *)

let print_map (map : map) : unit =
    for y = 0 to 5 do
        for x = 0 to 5 do
            let square = Hashtbl.find_opt map (x, y) in
            print_char
                (match square with
                | None -> ' '
                | Some square -> square)
        done ;
        print_newline ()
    done

let string_of_position (position : position) : string =
    let (x, y) = position in
    Printf.sprintf "(%d, %d)" x y

let string_of_heading (heading : heading) : string =
    match heading with
    | L -> "L"
    | U -> "U"
    | R -> "R"
    | D -> "D"

let string_of_viewpoint (viewpoint : viewpoint) : string =
    let (position, heading) = viewpoint in
    Printf.sprintf "%s %s"
        (string_of_position position)
        (string_of_heading heading)

(* Problem 1 *)

let next_heading (square : square) (heading : heading) : heading =
    match (square, heading) with
    | ('-', L) -> L | ('-', R) -> R
    | ('|', U) -> U | ('|', D) -> D
    | ('L', D) -> R | ('L', L) -> U
    | ('J', D) -> L | ('J', R) -> U
    | ('7', U) -> L | ('7', R) -> D
    | ('F', U) -> R | ('F', L) -> D
    | _ -> failwith "Invalid square"

let next_position (position : position) (heading : heading) : position =
    match heading with
    | L -> (fst position - 1, snd position)
    | U -> (fst position, snd position - 1)
    | R -> (fst position + 1, snd position)
    | D -> (fst position, snd position + 1)

let steps_in_cycle (map : map) (viewpoint : viewpoint) : int =
    let end_position = fst viewpoint in
    let rec loop (position, heading) count =
        let next_position = next_position position heading in
        if next_position = end_position
            then count + 1
            else
                let next_square = Hashtbl.find map next_position in
                let next_heading = next_heading next_square heading in
                loop (next_position, next_heading) (count + 1)
    in
    loop viewpoint 0

let problem1 () =
    start_viewpoint
    |> steps_in_cycle map
    |> fun count -> count / 2
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let problem2 () = ()

let () = problem1 () ; problem2 ()
