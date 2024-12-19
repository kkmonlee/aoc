open Core

type pattern = string
type design = string

let read_input filename =
  let content = In_channel.read_all filename in
  match String.split content ~on:'\n' 
        |> List.filter ~f:(fun s -> not (String.equal s "")) with
  | patterns :: designs -> 
      let patterns = String.split patterns ~on:',' 
                    |> List.map ~f:String.strip 
                    |> Set.of_list (module String) in
      let designs = designs |> List.map ~f:String.strip in
      (patterns, designs)
  | _ -> failwith "invalid input format"

(* p1 *)
let rec can_make_design patterns memo design =
  match design with
  | "" -> true
  | _ -> 
      match Hashtbl.find memo design with
      | Some result -> result
      | None ->
          let result = Set.exists patterns ~f:(fun pattern ->
            String.is_prefix design ~prefix:pattern &&
            can_make_design patterns memo (String.drop_prefix design (String.length pattern))
          ) in
          Hashtbl.set memo ~key:design ~data:result;
          result

(* p2 *)
let rec count_ways patterns memo design =
  match design with
  | "" -> 1
  | _ ->
      match Hashtbl.find memo design with
      | Some count -> count
      | None ->
          let count = Set.fold patterns ~init:0 ~f:(fun acc pattern ->
            if String.is_prefix design ~prefix:pattern then
              acc + count_ways patterns memo (String.drop_prefix design (String.length pattern))
            else
              acc
          ) in
          Hashtbl.set memo ~key:design ~data:count;
          count

let solve_both patterns designs =
  let memo1 = Hashtbl.create (module String) in
  let possible_count = List.count designs ~f:(can_make_design patterns memo1) in
  
  let memo2 = Hashtbl.create (module String) in
  let total_ways = List.fold designs ~init:0 ~f:(fun acc design ->
    acc + count_ways patterns memo2 design
  ) in
  
  (possible_count, total_ways)

let () =
  let (patterns, designs) = read_input "input/input_19.txt" in
  let (part1, part2) = solve_both patterns designs in
  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2