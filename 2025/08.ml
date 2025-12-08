let () =
  let boxes = In_channel.with_open_text "input/input_8.txt" In_channel.input_lines
    |> List.map (fun l -> String.split_on_char ',' l |> List.map int_of_string |> Array.of_list)
    |> Array.of_list in
  let n = Array.length boxes in
  
  let dist2 i j = 
    let b1, b2 = boxes.(i), boxes.(j) in
    (b1.(0)-b2.(0))*(b1.(0)-b2.(0)) + (b1.(1)-b2.(1))*(b1.(1)-b2.(1)) + (b1.(2)-b2.(2))*(b1.(2)-b2.(2)) in
  
  let num_pairs = n * (n-1) / 2 in
  let pairs = Array.make num_pairs (0, 0, 0) in
  let idx = ref 0 in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      pairs.(!idx) <- (dist2 i j, i, j); incr idx
    done
  done;
  Array.sort compare pairs;
  
  let parent = Array.init n Fun.id and rank = Array.make n 0 in
  let rec find x = if parent.(x) <> x then parent.(x) <- find parent.(x); parent.(x) in
  let unite a b =
    let pa, pb = find a, find b in
    if pa = pb then false else (
      let pa, pb = if rank.(pa) < rank.(pb) then pb, pa else pa, pb in
      parent.(pb) <- pa;
      if rank.(pa) = rank.(pb) then rank.(pa) <- rank.(pa) + 1;
      true) in
  
  for i = 0 to 999 do let (_, a, b) = pairs.(i) in ignore (unite a b) done;
  
  let sizes = Array.make n 0 in
  for i = 0 to n-1 do let r = find i in sizes.(r) <- sizes.(r) + 1 done;
  Array.sort (Fun.flip compare) sizes;
  Printf.printf "Part 1: %d\n" (sizes.(0) * sizes.(1) * sizes.(2));
  
  let circuits = ref (Array.fold_left (fun c s -> if s > 0 then c + 1 else c) 0 sizes) in
  let last = ref 0 in
  let i = ref 1000 in
  while !circuits > 1 do
    let (_, a, b) = pairs.(!i) in
    if unite a b then (decr circuits; last := !i);
    incr i
  done;
  
  let (_, a, b) = pairs.(!last) in
  Printf.printf "Part 2: %d\n" (boxes.(a).(0) * boxes.(b).(0))