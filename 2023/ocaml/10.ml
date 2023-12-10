#!/usr/bin/env ocaml

(* Model *)

type position = int * int
type heading = L | U | R | D
type viewpoint = position * heading
type square = char
type map = (position, square) Hashtbl.t
type turn = LT | RT
type step = turn option * viewpoint

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

let string_of_turn (turn : turn option) : string =
    match turn with
    | None -> "None"
    | Some turn -> match turn with
        | LT -> "LT"
        | RT -> "RT"

let string_of_step ((turn, (position, heading)) : step) : string =
    "-> "
    ^ string_of_position position
    ^ ", "
    ^ string_of_turn turn
    ^ " -> "
    ^ string_of_heading heading

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

let steps_in_cycle (map : map) (viewpoint : viewpoint) : step Seq.t =
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
    |> steps_in_cycle map
    |> Seq.length
    |> fun count -> count / 2
    |> string_of_int
    |> print_endline

(* Problem 2 *)

(*
Find the count of cells inside the route.

The set of cells inside the route is the set of cells that are in the map
    but not on the route itself
    and which have a membership score
        which is positive when the route is overall clockwise
        or negative if the route is overall counterclockwise.

The route is overall clockwise if it has more right turns than left turns.
Because all turns are at right angles and the path doesn't cross itself,
    it should be the case that the total of right turns minus left turns is
        4, indicating clockwise,
        or -4, indicating counterclockwise.
        other values indicate a mistake.

The membership score for a cell is the sum of the membership score for the cell's row and the membership score for the cell's column.

The membership score for a row or column is
    the number of turns on the route whose position and resulting heading, together called its viewpoint, would view the row or column as on the right
    minus those that would view the row or column as on the left.
That means that turns resulting in vertical headings affect the membership score of columns,
    and turns resulting in horizontal headings affect the membership score of rows.

Therefore, while following the route, we should collect into a sequence of traversed steps:
- the viewpoint of the step (position and heading)
- an optional turn, which is the direction of the turn if the step is a turn, or None if the step is not a turn

Given a sequence of such items, we can determine
- a filtered route of only the turns
    - the membership score of each row,
        by filtering the turns with horizontal viewpoints,
        then folding over them and accumulating
            +1 when the row is on the viewpoint's right
            and -1 when the row is on its left
    - the membership score of each column, similarly but with the vertical viewpoints
- the membership score of each cell, by adding the membership score of its row and column
- whether the route is overall clockwise or counterclockwise, by counting the turns
    - if the count is 4, the route is clockwise
    - if the count is -4, the route is counterclockwise
    - otherwise, the route is invalid
- the set of cells inside the route, by filtering the map for cells that are not on the route and whose membership score is positive if the route is clockwise or negative if the route is counterclockwise
- the count of cells inside the route, by counting the cells in the set

*)

let problem2 () =
    print_map map ;
    steps_in_cycle map start_viewpoint
    |> Seq.iter (fun step -> print_endline (string_of_step step))

let () = problem1 () ; problem2 ()
