(* Get the correct indices in the semi-matrix
   (using symetry to restrict storage to the bottom triangle) *)
let matrix_indices i j = if j < i then (i, j) else (j, i)

let matrix_value m i j =
  let i', j' = matrix_indices i j in
  m.(i').(j')

(* Executes a function only if the loop index is allowed by the indices array *)
let exec_filtered f indices =
  for i = 0 to Array.length indices - 1 do
    if indices.(i) then f i
  done

(* Find the minimum of an array considering only certain indices *)
let find_min_filtered t indices =
  let m = ref t.(0) in
  let imin = ref 0 in
  exec_filtered (fun i ->
    if t.(i) < !m then begin
      m := t.(i);
      imin := i
    end
  ) indices;
  !m, !imin

(* Calculate the distance between two points stored in a list in an array (for algorithm initialization) *)
let distance points a b =
  match a, b with
  | [x], [y] -> Utils.distance points.(x) points.(y)
  | _ -> failwith "invalid input"

(* Compute the initial dissimilarity matrix for the HAC algorithm *)
let compute_dissimilarity points classes =
  let n = Array.length classes in
  let dissim = Array.init n (fun i -> Array.make i 0.) in
  let nearest = Array.init n (fun i -> i) in
  let dist_to_nearest = Array.make n infinity in

  for i = 1 to n - 1 do
    for j = 0 to i - 1 do
      dissim.(i).(j) <- distance points classes.(i) classes.(j);

      if dissim.(i).(j) < dist_to_nearest.(i) then begin
        nearest.(i) <- j;
        dist_to_nearest.(i) <- dissim.(i).(j)
      end;
      if dissim.(i).(j) < dist_to_nearest.(j) then begin
        nearest.(j) <- i;
        dist_to_nearest.(j) <- dissim.(i).(j)
      end
    done
  done;
  dissim, nearest, dist_to_nearest

let print_list_array a =
  Array.iter (fun l ->
    print_string "[ ";
    List.iter (fun i -> Printf.printf "%d " i) l;
    print_string "]\n"
  ) a

(* Main function *)
let ahc threshold pts =
  let classes = Array.init (Array.length pts) (fun i -> [i]) in
  let n = Array.length classes in
  (* 1. Compute the similarity matrix and the two helper arrays *)
  let dissim, nearest, dist_to_nearest = compute_dissimilarity pts classes in
  let indices = Array.make n true in
  let nb_removed = ref 0 in

  (* 2. Main execution loop - until the threshold is reached *)
  while n - !nb_removed > threshold do
    Printf.printf "\rRemoved %d points" !nb_removed;
    (* 3. Find the two nearest classes *)
    let (_, imin) = find_min_filtered dist_to_nearest indices in
    let (i, j) = matrix_indices imin nearest.(imin) in

    (* 4. Update the dissimilarity matrix for the merge of i and j (all distances from (or to) i updated) *)
    (*    and the arrays with the new neighbours *)
    indices.(j) <- false;
    incr nb_removed;
    let min_k = ref (-1) in
    let min_d = ref infinity in
    exec_filtered (fun k ->
      if k <> i then begin
        let i', k' = matrix_indices i k in
        let m = min (matrix_value dissim i k) (matrix_value dissim j k) in
        if m < !min_d then begin
          min_d := m;
          min_k := k
        end;

        dissim.(i').(k') <- m;
        if nearest.(k) = j then nearest.(k) <- i
      end
    ) indices;
    classes.(i) <- classes.(i) @ classes.(j);
    classes.(j) <- [];
    nearest.(i) <- !min_k;
    dist_to_nearest.(i) <- !min_d;
    dist_to_nearest.(j) <- infinity
  done;
  (*print_list_array classes;*)

  (* 6. Give back the classes found *)
  let merged = Array.make (n - !nb_removed) [] in
  let k = ref 0 in
  let i = ref 0 in
  while !i < Array.length merged do
    if indices.(!i + !k) then begin
      merged.(!i) <- classes.(!i + !k);
      incr i
    end
    else incr k
  done;

  merged
