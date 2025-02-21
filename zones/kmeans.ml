open Utils

let distance_to_center pt (xc, yc) = sqrt ((pt.x -. xc) ** 2. +. (pt.y -. yc) ** 2.)

let swap t a b =
  let temp = t.(a) in
  t.(a) <- t.(b);
  t.(b) <- temp

(* get the first k random points to start the algorithm *)
let get_random_points k points =
  let n = Array.length points in
  Random.self_init ();
  let centers = Array.make k (0., 0.) in
  let indices = Array.make k (-1) in

  (* Randomly choosing centers by putting them at the beginning of the table, *)
  (* selecting them randomly in the rest *)
  for i = 0 to k - 1 do
    indices.(i) <- Random.int (n - i) + i;
    swap points i indices.(i);
    centers.(i) <- points.(indices.(i)).x, points.(indices.(i)).y
  done;

  (* reverting the changes made to choose the centers *)
  for i = k - 1 downto 0 do
    swap points i indices.(i)
  done;

  centers

(* calculate the index of the closest center from a point *)
let closest_center pt centers = 
  let dmin = ref (distance_to_center pt centers.(0)) in
  let cmin = ref 0 in
  for c = 1 to Array.length centers - 1 do
    let dist = distance_to_center pt centers.(c) in
    if dist < !dmin then begin
      dmin := dist;
      cmin := c
    end
  done;
  !cmin

(* replace every center by the barycenter of the points they are assigned to *)
let update_centers points centers refs = 
  let n = Array.length points in
  let k = Array.length centers in
  let nb_points = Array.make k 0. in
  for c = 0 to k - 1 do
    centers.(c) <- (0., 0.)
  done;
  for i = 0 to n - 1 do
    let c = refs.(i) in
    let (xc, yc) = centers.(c) in
    centers.(c) <- (xc +. points.(i).x *. points.(i).weight, yc +. points.(i).y *. points.(i).weight);
    nb_points.(c) <- nb_points.(c) +. points.(i).weight
  done;
  for c = 0 to k - 1 do
    if nb_points.(c) > 0. then 
      let (xc, yc) = centers.(c) in
      centers.(c) <- xc /. nb_points.(c), yc /. nb_points.(c)
  done

let update_refs points centers refs =
  let has_changed = ref false in
  for i = 0 to Array.length points - 1 do
    let cmin = closest_center points.(i) centers in
    if refs.(i) <> cmin then begin
      has_changed := true;
      refs.(i) <- cmin
    end
  done;
  !has_changed

let kmeans points nb_zones rolls =
  let n = Array.length points in
  let centers = get_random_points nb_zones points in
  let refs = Array.make n 0 in

  let i = ref 0 in
  while !i < rolls && update_refs points centers refs do
    update_centers points centers refs;
    incr i
  done;
  Printf.printf "Convergence in %d iterations\n" !i;
  refs, nb_zones
