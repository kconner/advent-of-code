#!/usr/bin/env ocaml

(* Model *)

type row = int Seq.t

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

let line_list (line : string) : int list =
    line
    |> String.split_on_char ' '
    |> List.map int_of_string

let lists = with_file "9.txt" file_lines |> List.map line_list

(* Problem 1 *)

let row_from_end (list : int list) : row = 
    List.fold_left (fun acc item -> Seq.cons item acc) Seq.empty list

let row_of_differences (row : row) : row =
    match Seq.uncons row with
    | None -> Seq.empty
    | Some pair ->
        Seq.unfold
            (fun (prev, row) ->
                match Seq.uncons row with
                | None -> None
                | Some (hd, tl) -> Some (prev - hd, (hd, tl)))
            pair

let next_number (row : row) : int =
    row
    |> Seq.unfold (fun row -> Some (row, row_of_differences row))
    |> Seq.map (fun row -> Seq.uncons row |> Option.map fst)
    |> Seq.take_while (fun item -> item != Option.none)
    |> Seq.map Option.get
    |> Seq.fold_left (+) 0

let sum_of_next_numbers (rows : row list) : int =
    rows
    |> List.map next_number
    |> List.fold_left (+) 0

let problem1 () : unit =
    lists
    |> List.map row_from_end
    |> sum_of_next_numbers
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let problem2 () : unit =
    lists
    |> List.map (fun list -> list |> List.rev |> row_from_end)
    |> sum_of_next_numbers
    |> string_of_int
    |> print_endline

let () = problem1 () ; problem2 ()
