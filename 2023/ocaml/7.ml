#!/usr/bin/env ocaml

(* Model *)

type card = char
type hand = string
type item = (hand * int)
type card_frequencies = (card, int) Hashtbl.t
type score = string
type scored_item = (hand * int * score)
type frequency_frequencies = (int, int) Hashtbl.t

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

let line_item (line : string) : item =
    let words = String.split_on_char ' ' line in
    (List.hd words, List.nth words 1 |> int_of_string)

let items = with_file "7.txt" file_lines |> List.map line_item

(* Problem 1 *)

let hand_card_frequencies (h : hand) : card_frequencies = 
    let freqs = Hashtbl.create 13 in
    let add_card (c : card) : unit =
        let freq = try Hashtbl.find freqs c with Not_found -> 0 in
        Hashtbl.replace freqs c (freq + 1)
    in
    String.iter add_card h ;
    freqs

let n_of_a_kind_count (n : int) (f : card_frequencies) : char =
    Hashtbl.fold (fun _ v acc ->
        Char.chr (Char.code acc + (if v = n then 1 else 0)))
        f
        '0'

let card_value (c : card) : char =
    match c with
    | 'A' -> 'E'
    | 'K' -> 'D'
    | 'Q' -> 'C'
    | 'J' -> 'B'
    | 'T' -> 'A'
    | _ -> c

let hand_score (h : hand) : score =
    let freqs = hand_card_frequencies h in
    let type_score = Seq.init 4 (fun i -> i + 2)
        |> Seq.map (fun i -> n_of_a_kind_count i freqs) in
    let card_score = Seq.init 5 (fun i -> String.get h i
        |> card_value) in
    Seq.fold_left
        (fun acc x -> Seq.cons x acc)
        card_score
        type_score
    |> String.of_seq

let compare_scored_items ((_, _, a) : scored_item) ((_, _, b) : scored_item) : int =
    String.compare a b

let problem1 () : unit =
    items
    |> List.map (fun (h, b) -> (h, b, hand_score h))
    |> List.sort compare_scored_items
    |> List.mapi (fun i (_, b, _) -> b * (i + 1))
    |> List.fold_left (+) 0
    |> string_of_int
    |> print_endline

(* Problem 2 *)

let frequency_frequencies (f : card_frequencies) : frequency_frequencies =
    let ff = Hashtbl.create 13 in
    Hashtbl.iter (fun _ k ->
        let v = Hashtbl.find_opt ff k |> Option.value ~default:0 in
        Hashtbl.replace ff k (v + 1))
        f ;
    ff

let card_value_with_wilds (c : card) : char =
    match c with
    | 'J' -> '1'
    | _ -> card_value c

let frequency_frequency (ff : frequency_frequencies) (f : int) : int =
    Hashtbl.find_opt ff f |> Option.value ~default:0

let kind_score_char (b : bool) : char =
    Char.chr (Char.code '0' + if b then 1 else 0)

let hand_score_with_wilds (h : hand) : score =
    let f = hand_card_frequencies h in
    let wild_count = Hashtbl.find_opt f 'J' |> Option.value ~default:0 in
    Hashtbl.remove f 'J' ;
    let fff = frequency_frequencies f |> frequency_frequency in
    let type_score = match 0 with
        | _ when wild_count >= 4 || fff (5 - wild_count) = 1 -> '6'
        | _ when wild_count >= 3 || fff (4 - wild_count) = 1 -> '5'
        | _ when wild_count = 1 && fff 2 = 2 || fff 3 = 1 && fff 2 = 1 -> '4'
        | _ when wild_count >= 2 || fff (3 - wild_count) = 1 -> '3'
        | _ when wild_count >= 1 && fff 2 >= 1 || fff 2 = 2 -> '2'
        | _ when wild_count >= 1 || fff 2 = 1 -> '1'
        | _ -> '0'
        in
    let card_score = Seq.init 5 (fun i -> String.get h i
        |> card_value_with_wilds) in
    Seq.cons type_score card_score |> String.of_seq

let problem2 () : unit =
    items
    |> List.map (fun (h, b) -> (h, b, hand_score_with_wilds h))
    (* |> List.iter (fun (h, _, s) -> print_endline (h ^ " " ^ s)) *)
    |> List.sort compare_scored_items
    |> List.mapi (fun i (_, b, _) -> b * (i + 1))
    |> List.fold_left (+) 0
    |> string_of_int
    |> print_endline

let () = problem1 () ; problem2 ()
