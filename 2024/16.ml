type point = { x: int; y: int }
type dir = Up | Right | Down | Left
type state = { pos: point; dir: dir }

module PSet = Set.Make(struct
  type t = point
  let compare p1 p2 =
    match compare p1.x p2.x with
    | 0 -> compare p1.y p2.y
    | c -> c
end)

module PQ = struct
  type t = {
    mutable heap: (int * state * PSet.t) array;
    mutable size: int
  }

  let create () = { heap = Array.make 10000 (0, { pos = {x=0; y=0}; dir = Up }, PSet.empty); size = 0 }

  let swap h i j =
    let temp = h.heap.(i) in
    h.heap.(i) <- h.heap.(j);
    h.heap.(j) <- temp

  let rec bubble_up h i =
    if i = 0 then () else
    let parent = (i - 1) / 2 in
    let (score_i, _, _) = h.heap.(i) in
    let (score_p, _, _) = h.heap.(parent) in
    if score_i < score_p then begin
      swap h i parent;
      bubble_up h parent
    end

  let rec bubble_down h i =
    let left = 2 * i + 1 in
    let right = 2 * i + 2 in
    if left >= h.size then () else
    let min_child =
      if right >= h.size then left
      else let (score_l, _, _) = h.heap.(left) in
           let (score_r, _, _) = h.heap.(right) in
           if score_l <= score_r then left else right in
    let (score_i, _, _) = h.heap.(i) in
    let (score_m, _, _) = h.heap.(min_child) in
    if score_m < score_i then begin
      swap h i min_child;
      bubble_down h min_child
    end

  let push h item =
    if h.size = Array.length h.heap then begin
      let new_heap = Array.make (h.size * 2) (0, { pos = {x=0; y=0}; dir = Up }, PSet.empty) in
      Array.blit h.heap 0 new_heap 0 h.size;
      h.heap <- new_heap
    end;
    h.heap.(h.size) <- item;
    bubble_up h h.size;
    h.size <- h.size + 1

  let pop h =
    if h.size = 0 then None else begin
      let result = h.heap.(0) in
      h.size <- h.size - 1;
      if h.size > 0 then begin
        h.heap.(0) <- h.heap.(h.size);
        bubble_down h 0
      end;
      Some result
    end
end

let dir_to_vec = function
  | Up -> { x = 0; y = -1 }
  | Right -> { x = 1; y = 0 }
  | Down -> { x = 0; y = 1 }
  | Left -> { x = -1; y = 0 }

let turn_left = function
  | Up -> Left | Left -> Down | Down -> Right | Right -> Up

let turn_right = function
  | Up -> Right | Right -> Down | Down -> Left | Left -> Up

let (+:) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }

let solve input =
  let grid = Array.of_list (List.filter ((<>) "") (String.split_on_char '\n' input)) in
  let h, w = Array.length grid, String.length grid.(0) in
  
  let find_pos c =
    let rec scan_row y =
      if y >= h then failwith "not found"
      else match String.index_opt grid.(y) c with
        | Some x -> { x; y }
        | None -> scan_row (y + 1)
    in scan_row 0
  in
  
  let start, goal = find_pos 'S', find_pos 'E' in
  let valid p = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h && grid.(p.y).[p.x] <> '#' in
  
  let rec find_paths queue seen best_score paths =
    match PQ.pop queue with
    | None -> (best_score, paths)
    | Some (score, state, visited) ->
        let current_best = Hashtbl.find_opt seen state |> Option.value ~default:max_int in
        if score > best_score || score > current_best then
          find_paths queue seen best_score paths
        else if state.pos = goal then
          let new_paths = if score <= best_score then
                           if score < best_score then [visited]
                           else visited :: paths
                         else paths in
          find_paths queue seen (min score best_score) new_paths
        else begin
          Hashtbl.replace seen state score;
          let forward = state.pos +: dir_to_vec state.dir in
          if valid forward then
            PQ.push queue (score + 1, { state with pos = forward }, 
                          PSet.add forward (PSet.add state.pos visited));
          PQ.push queue (score + 1000, { state with dir = turn_left state.dir }, 
                        PSet.add state.pos visited);
          PQ.push queue (score + 1000, { state with dir = turn_right state.dir }, 
                        PSet.add state.pos visited);
          find_paths queue seen best_score paths
        end
  in
  
  let queue = PQ.create () in
  let initial = { pos = start; dir = Right } in
  PQ.push queue (0, initial, PSet.singleton start);
  
  let score, paths = find_paths queue (Hashtbl.create 1000) max_int [] in
  let all_tiles = List.fold_left PSet.union PSet.empty paths in
  
  (score, PSet.cardinal all_tiles)

let () =
  let input = In_channel.input_all (open_in "input/input_16.txt") in
  let score, tiles = solve input in
  Printf.printf "Part 1: %d\n" score;
  Printf.printf "Part 2: %d\n" tiles