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
type line_membership_scores = int array

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

(* Debugging *)

let print_map (map : map) ((mx, my) : dimensions) : unit =
    for y = 0 to my do
        for x = 0 to mx do
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

let string_of_side (side : path_side) : string =
    match side with
    | LH -> "LH"
    | RH -> "RH"

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

let line_membership_scores (map : map) ((mx, my) : dimensions) (path : path) (inside : path_side) : (line_membership_scores * line_membership_scores) =
    let turn_viewpoints = path
        |> Seq.filter (fun (turn, _) -> turn != None)
        |> Seq.map snd in
    let (horizontal_viewpoints, vertical_viewpoints) = turn_viewpoints
        |> Seq.partition (fun (_, heading) -> heading = L || heading = R) in
    let negate_when_inside_is_left (count : int) : int =
        if inside == LH then -count else count in
    let row_score (y : int) : int =
        horizontal_viewpoints
        |> Seq.map (fun ((_, ty), heading) ->
            match (ty - y, heading) with
            | (0, _) -> 0
            | (dy, L) when dy < 0 -> -1
            | (dy, L) when 0 < dy -> +1
            | (dy, R) when dy < 0 -> +1
            | (dy, R) when 0 < dy -> -1
            | _ -> failwith "Invalid turn viewpoint")
        |> Seq.fold_left (+) 0
        |> negate_when_inside_is_left in
    let column_score (x : int) : int =
        vertical_viewpoints
        |> Seq.map (fun ((tx, _), heading) ->
            match (tx - x, heading) with
            | (0, _) -> 0
            | (dx, U) when dx < 0 -> +1
            | (dx, U) when 0 < dx -> -1
            | (dx, D) when dx < 0 -> -1
            | (dx, D) when 0 < dx -> +1
            | _ -> failwith "Invalid turn viewpoint")
        |> Seq.fold_left (+) 0
        |> negate_when_inside_is_left in
    (Array.init my row_score, Array.init mx column_score)

let non_path_positions (map : map) (path : path) : (position, unit) Hashtbl.t =
    let non_path_positions = Hashtbl.create 400000 in
    Hashtbl.iter (fun position _ -> Hashtbl.add non_path_positions position ()) map ;
    path |> Seq.iter (fun (_, (position, _)) -> Hashtbl.remove non_path_positions position) ;
    non_path_positions

(*
Find the count of cells inside the path.

The set of cells inside the path is the set of cells that are in the map
    but not on the path itself
    and which have a membership score
        which is positive when the path's inside is to the right
        or negative if the path's inside is to the left.

The path's inside is to the right if it has more right turns than left turns.
Because all turns are at right angles and the path doesn't cross itself,
    it should be the case that the total of right turns minus left turns is
        4, indicating inside to the right,
        or -4, indicating inside to the left.
        other values indicate a mistake.

The membership score for a cell is the sum of the membership score for the cell's row and the membership score for the cell's column.

The membership score for a row or column is
    the number of turns on the path whose position and resulting heading, together called its viewpoint, would view the row or column as on the right
    minus those that would view the row or column as on the left.
That means that turns resulting in vertical headings affect the membership score of columns,
    and turns resulting in horizontal headings affect the membership score of rows.

Therefore, while following the path, we should collect into a sequence of traversed steps:
- the viewpoint of the step (position and heading)
- an optional turn, which is the direction of the turn if the step is a turn, or None if the step is not a turn

Given a sequence of such items, we can determine
- whether the inside of the path is to the right or left, by counting the turns
    - if the count is 4, the inside is on the right
    - if the count is -4, the inside is on the left
    - otherwise, the path is invalid
- the membership score of each row,
    by filtering the turns with horizontal viewpoints,
    then folding over them and accumulating
        +1 when the row is on the inside side
        and -1 when the row is on the outside side
- the membership score of each column, similarly but with the vertical viewpoints
- the cells not in the path, by copying the map and removing cells that are on the path
- the membership score of each cell not in the path, by adding the membership score of its row and column
- the set of cells inside the path, by filtering the map for cells that are not on the path and whose membership score is positive
    (or is it, those that are at least 4?)
- the count of cells inside the path, by counting the cells in the set

*)

let problem2 () =
    let path = path_from_start map start_viewpoint in
    let inside = path_inside path in
    let (row_scores, column_scores) = line_membership_scores map dimensions path inside in
    let non_path_positions = non_path_positions map path in
    let cell_scores = Hashtbl.fold
        (fun (x, y) _ list ->
            (row_scores.(y) + column_scores.(x)) :: list)
        non_path_positions
        [] in

    cell_scores
    |> List.filter (fun score -> 0 < score)
    |> List.length
    |> string_of_int
    |> print_endline ;

    print_newline () ;
    row_scores |> Array.iter (fun score -> string_of_int score |> print_endline) ;
    print_newline () ;
    column_scores |> Array.iter (fun score -> string_of_int score |> print_endline) 

let () = problem1 () ; problem2 ()
