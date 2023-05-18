(* naïve method for the clustering algorithm, which proved totally inefficient *)

open Utils

(* Calculate the simple distance between two classes : *)
(* min for x in a and y in b of the euclidian distance between a and b *)
let simple_distance a b =
  let rec explore_a a m =
    match a with
    | [] -> m
    | pointA :: a' -> begin
      let rec explore_b b m =
        match b with
        | [] -> explore_a a' m
        | pointB :: b' ->
          min (Utils.distance pointA pointB) m
          |> explore_b b'
        in explore_b b m
    end
  in explore_a a infinity

(* Calculate the proximity matrix of classes *)
let proximity classes =
  let n = Array.length classes in
  let mp = Array.make_matrix n n 0. in
  for a = 0 to n - 1 do
    for b = 0 to n - 1 do
      if a <> b then mp.(a).(b) <- simple_distance classes.(a) classes.(b)
    done
  done;
  mp

(* Find the two nearest classes in terms of simple distance*)
let nearest_classes t =
  let mp = proximity t in
  let n = Array.length t in

  let rec explore i a b d =
    if i = n * n then (a, b, d) else
    let a' = i mod 4 in
    let b' = i / 4 in
    let d' = mp.(a').(b') in
    if d' < d then explore (i + 1) a' b' d'
    else explore (i + 1) a b d
  in explore 1 0 0 mp.(0).(0)

(* Agglomerative hierarchical clustering algorithm *)
let ahc pts threshold =
  Printf.printf "Starting clustering algorithm\n";
  let rec aux tab =
    let n = Array.length tab in
    if n <= threshold then tab else
      let a, b, _ = nearest_classes tab in
      if n mod 1000 = 0 then Printf.printf "\rRemaining : %d!" n;
      Array.init (n - 1) (fun i ->
        if i = a then tab.(a) @ tab.(b)
        else if i >= b then tab.(i + 1)
        else tab.(i)
      )
      |> aux
  in
  Array.of_list pts
  |> Array.map (fun x -> [x])
  |> aux

(* Calculate the barycenter of a points list *)
let calculate_barycenter pts =
  let rec aux xc yc wc n = function
    | [] -> Utils.new_parsed_point (xc/.n) (yc/.n) wc
    | point :: xs -> aux (xc +. point.x) (yc +. point.y) (wc +. point.weight) (n +. 1.) xs
  in aux 0. 0. 0. 0. pts

(* Full clustering algorithm *)
let gather_points threshold pts =
  let classes = ahc pts threshold in
  let n = Array.length classes in

  let rec aux i acc =
    if i = n then acc else
    aux (i + 1) (calculate_barycenter classes.(i) :: acc)
  in aux 0 []
