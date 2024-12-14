(* ocamlc -I +str str.cma 14.ml -o 14 *)
open Printf
open Str

type robot = { mutable x : int; mutable y : int; vx : int; vy : int }

let parse_robot line =
  try
    let re = Str.regexp "p=\\([-0-9]+\\),\\([-0-9]+\\) v=\\([-0-9]+\\),\\([-0-9]+\\)" in
    if Str.string_match re line 0 then
      {
        x = int_of_string (Str.matched_group 1 line);
        y = int_of_string (Str.matched_group 2 line);
        vx = int_of_string (Str.matched_group 3 line);
        vy = int_of_string (Str.matched_group 4 line);
      }
    else
      failwith "malformed robot input"
  with _ -> failwith "failed to parse"

let init_robots lines = List.map parse_robot lines

(* p2 *)
let simulate_motion robots size =
  let steps = ref 0 in
  let seen_positions = Hashtbl.create (List.length robots) in
  let max_steps = 1_000_000 in (* TODO: limit? *)
  try
    while !steps < max_steps do
      incr steps;
      Hashtbl.clear seen_positions;
      List.iter
        (fun r ->
          r.x <- (r.x + r.vx) mod fst size;
          r.y <- (r.y + r.vy) mod snd size;
          if r.x < 0 then r.x <- r.x + fst size;
          if r.y < 0 then r.y <- r.y + snd size;
          Hashtbl.replace seen_positions (r.x, r.y) ())
        robots;
      if Hashtbl.length seen_positions = List.length robots then
        raise (Failure (string_of_int !steps));
    done;
    failwith "failed to converge within limits"
  with Failure steps -> int_of_string steps


(* p1 *)
let calculate_quadrants robots size =
  let half_x, half_y = (fst size / 2, snd size / 2) in
  let q1 = ref 0 and q2 = ref 0 and q3 = ref 0 and q4 = ref 0 in
  List.iter
    (fun r ->
      if r.x < half_x && r.y < half_y then incr q1
      else if r.x >= half_x && r.y < half_y then incr q2
      else if r.x < half_x && r.y >= half_y then incr q3
      else if r.x >= half_x && r.y >= half_y then incr q4)
    robots;
  !q1 * !q2 * !q3 * !q4

let calc mode lines =
  let size = (101, 103) in
  let robots = init_robots lines in
  match mode with
  | 1 -> calculate_quadrants robots size
  | 2 -> simulate_motion robots size
  | _ -> failwith "fn -> mode - invalid"

let read_file filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic; List.rev acc
  in
  loop []

let () =
  let infile = "input/input_14.txt" in
  let lines = read_file infile in
  Printf.printf "Part 1: %d\n" (calc 1 lines);
  Printf.printf "Part 2: %d\n" (calc 2 lines)
