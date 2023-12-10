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

let map_and_start (lines : string list) : (map * position) =
    let map = Hashtbl.create 400000 in
    let start = ref (-1, -1) in
    List.iteri
        (fun y line ->
            String.iteri
                (fun x square ->
                    Hashtbl.add map (x, y) square ;
                    if square = 'S' then start := (x, y))
                line)
        lines ;
    (map, !start)

let (map, start_position) = with_file "10.txt" file_lines |> map_and_start

let start_heading =
    [(-1, 0, "-LF", L);
        (0, -1, "|7F", U);
        (1, 0, "-J7", R);
        (0, 1, "|LJ", D)]
    |> List.find
        (fun (dx, dy, filter, heading) ->
            let adjacent = (fst start_position + dx, snd start_position + dy) in
            match Hashtbl.find_opt map adjacent with
            | None -> false
            | Some square -> String.contains filter square)
    |> fun (_, _, _, heading) -> heading

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

let steps_to_start (map : map) (viewpoint : viewpoint) : int =
    let rec loop (position, heading) count =
        let next_position = next_position position heading in
        if next_position = start_position
            then count + 1
            else
                let next_square = Hashtbl.find map next_position in
                let next_heading = next_heading next_square heading in
                loop (next_position, next_heading) (count + 1)
    in
    loop viewpoint 0

let problem1 () =
    (start_position, start_heading)
    |> steps_to_start map
    |> fun count -> count / 2
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let problem2 () = ()

let () = problem1 () ; problem2 ()
